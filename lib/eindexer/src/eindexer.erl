%%%-------------------------------------------------------------------
%%% File    : eindexer.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  1 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(eindexer).

-behaviour(gen_server).

%% API
-export([start_link/0, index/2, reindex/0, run_query/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {directory, docs, terms, doc_term, idf, trigrams}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Indexes all files below the file location provided.
%% @spec (Type, Location::string()) -> void()
%% where
%%  Type = text_files | edocs
%% @end
%%--------------------------------------------------------------------
index (Type, Loc) ->
    gen_server:cast(?SERVER, {index, {Type, Loc}}).

%%--------------------------------------------------------------------
%% @private
%% @doc Indexes all files below the file location provided 
%% @spec () -> void()
%% @end
%%--------------------------------------------------------------------
reindex () ->
    gen_server:call(?SERVER, reindex).

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @spec run_query(QueryString) -> Results
%% where
%%  Results = [Result]
%%   Result = {AppName, {integer(), AppVsn}}
%% @end
%%--------------------------------------------------------------------
run_query (QueryString) ->
    gen_server:call(?SERVER, {run_query, QueryString}).

%%====================================================================
%% gen_server callbacks
%%====================================================================


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Docs = ets:new(docs, [bag]),
    Terms = ets:new(terms, []),
    TermDoc = ets:new(doc_term, [bag]),
    IDF = ets:new(idf, []),
    EtsTrigrams = indexer_trigrams:open(),
    
    {ok, #state{docs=Docs, terms=Terms, doc_term=TermDoc, idf=IDF, trigrams=EtsTrigrams}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(reindex, _From, State) ->
    {reply, ok, State};
handle_call({run_query, Query}, _From, State) ->
    Terms = utils:clean (Query, State#state.trigrams),
    
    FinalRankings = lists:foldl (fun (Term, Rankings) ->
                                         NewRankings = case ets:lookup (State#state.idf, Term) of
                                             [{_, IDF}] ->
                                                 Results = ets:lookup (State#state.doc_term, Term),  
                                                 lists:foldl (fun ({_, Entry, TF}, NewRankings) ->
                                                                      dict:update (Entry, fun({Old, Vsn}) -> {Old + TF*IDF, Vsn} end, {TF*IDF, hd(hd(ets:match(State#state.docs, {application, Entry, '_', '$1', '_'})))}, NewRankings)
                                                              end, Rankings, Results);
                                             [] ->
                                                 Rankings
                                         end,
                                         PossibleApp = list_to_atom(binary_to_list(Term)),
                                         Results2 = ets:match (State#state.docs, {application, PossibleApp, '_', '$1', '_'}),
                                         case Results2 of
                                             [] ->
                                                 NewRankings;
                                             [[Vsn]] ->
                                                 dict:update (PossibleApp, fun({Old, _Vsn}) -> {Old + 100, Vsn} end, {100, Vsn}, NewRankings)
                                         end
                                 end, dict:new(), Terms),
    RankingsList = lists:reverse (lists:keysort (2, dict:to_list (FinalRankings))),
    
    %io:format ("Results:~n"),
    %lists:map (fun({Entry, Ranking}) ->
    %                   io:format("~s : ~p~n", [Entry, Ranking])
    %           end, RankingsList),
    {reply, RankingsList, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({index, {Type, Loc}}, State) ->
    {_, {H, M, S}} = calendar:local_time(),
    StartSeconds = 360*H + M*60 + S,
    case Type of
        text_files ->
            text_file_indexer:index (Loc, State#state.docs, State#state.doc_term, State#state.terms, State#state.idf, State#state.trigrams);
        edocs ->
            edoc_indexer:index (Loc, State#state.docs, State#state.doc_term, State#state.terms, State#state.idf, State#state.trigrams)
    end,
    {_, {H2, M2, S2}} = calendar:local_time(),
    FinishSeconds = 360*H2 + M2 *60 + S2,
    io:format ("~nTime to Index: ~p seconds~n", [FinishSeconds - StartSeconds]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

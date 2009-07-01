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

-record(state, {directory, docs, terms, doc_term, idf}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

index (Type, Loc) ->
    gen_server:call(?SERVER, {index, Type, Loc}).

reindex () ->
    gen_server:call(?SERVER, {reindex}).

run_query (Terms) ->
    gen_server:call(?SERVER, {run_query, Terms}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Docs = ets:new(docs, []),
    Terms = ets:new(terms, []),
    TermDoc = ets:new(doc_term, [bag]),
    IDF = ets:new(idf, []),
    
    {ok, #state{docs=Docs, terms=Terms, doc_term=TermDoc, idf=IDF}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({index, Type, Loc}, _From, State) ->
    internal_index (Type, Loc, State),
    
    {reply, ok, State};
handle_call({reindex}, _From, State) ->
    {reply, ok, State};
handle_call({run_query, Query}, _From, State) ->
    Terms = utils:clean (Query),
    
    FinalRankings = lists:foldl (fun (Term, Rankings) ->
                             [{_, IDF}] = ets:lookup (State#state.idf, Term),
                             Results = ets:lookup (State#state.doc_term, Term),
                             lists:foldl (fun ({_, Entry, TF}, NewRankings) ->
                                                  dict:update (Entry, fun(Old) -> Old + TF*IDF end, TF*IDF, NewRankings)
                                          end, Rankings, Results)
                     end, dict:new(), Terms),
    RankingsList = lists:reverse (lists:keysort (2, dict:to_list (FinalRankings))),
    
    %io:format ("Results:~n"),
    %lists:map (fun({Entry, Ranking}) ->
    %                   io:format("~s : ~p~n", [Entry, Ranking])
    %           end, RankingsList),
    {reply, RankingsList, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
internal_index (text_files, Dir, State) ->
    Files = utils:generate_file_listing(Dir),    
    NumDocs = lists:foldl (fun (AbsolutePath, Acc) ->
                                   ets:insert (State#state.docs, {AbsolutePath, AbsolutePath}),
                                   case file:read_file(AbsolutePath) of
                                       {ok, Binary} ->
                                           insert_words(binary_to_list(Binary), AbsolutePath, State#state.terms, State#state.doc_term),
                                           Acc+1;
                                       {error, _Reason} ->
                                           Acc
                                   end
                           end, 0, Files),
    create_idf_table (NumDocs, State#state.terms, State#state.doc_term, State#state.idf).

insert_words (File, Entry, TermTable, EntryTermTable) ->
    Words = utils:clean (File),    
    lists:foreach (fun (Word) ->
                           term_frequency (Entry, Word, TermTable, EntryTermTable)
                   end, Words).

term_frequency (Entry, Word, TermTable, EntryTermTable) ->
    ets:insert(TermTable, {Word}),
    case ets:match(EntryTermTable, {Word, Entry, '$1'}) of
        [] ->
            ets:insert(EntryTermTable, {Word, Entry, 1});
		    [[TF]] ->
            ets:delete_object(EntryTermTable, {Word, Entry, TF}),
            ets:insert(EntryTermTable, {Word, Entry, TF+1})
    end.
    
create_idf_table (NumDocs, TermsTable, EntryTermTable, IDFTable) ->
    ets:foldl (fun ({Term}, _Acc) ->
                       ets:insert (IDFTable, {Term, math:log(NumDocs / length(ets:lookup(EntryTermTable, Term)))})
               end, 0, TermsTable),
    ok.

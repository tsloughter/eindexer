%%%-------------------------------------------------------------------
%%% File    : utils.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  1 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(utils).
-export([term_frequency/4, create_idf_table/4, edoc_clean/2, clean/2, edoc_generate_file_listing/1, generate_file_listing/1, get_overview_file/1, app_generate_file_listing/1]).

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

generate_file_listing(Dir) ->
    filelib:fold_files(Dir, ".+", true, fun(F, AccIn) -> [F | AccIn] end, []).

edoc_generate_file_listing(Dir) ->
    filelib:fold_files(Dir, ".+([\.]erl)$", true, fun(F, AccIn) -> [F | AccIn] end, []).

get_overview_file(Dir) ->
    filelib:fold_files(Dir, "(overview.edoc)$", true, fun(F, AccIn) -> [F | AccIn] end, []).

app_generate_file_listing(Dir) ->
    filelib:fold_files(Dir, ".+([\.]app)$", true, fun(F, AccIn) -> [F | AccIn] end, []).

%%% Strips all punctuation from the beginning and end of a word
%% strip_all (Word, []) ->
%%     Word;
%% strip_all (Word, [H | T]) ->	
%%     strip_all(string:strip(Word, both, H), T).
%% strip_all(Word) ->
%%     strip_all(Word, [$., $,, $;, $*]). 

to_lower_case([H|T]) when $A=< H, H=<$Z -> [H+$a-$A|to_lower_case(T)];
to_lower_case([H|T])                    -> [H|to_lower_case(T)];
to_lower_case([])                       -> [].

process_word(Word, EtsTrigrams) when length(Word) < 20 ->
    Word1 = to_lower_case(Word),
    case stop(Word1) of
        true  -> no;
        false ->
            case indexer_trigrams:is_word(EtsTrigrams, Word1) of
                true ->
                    Word2 = porter:stem(Word1),
                    if 
                        length(Word2) < 3 -> no;
                        true              -> {yes, Word2}
                    end;
                false -> no
            end
    end;
process_word(_, _) ->
    no.

stem_split_line(Line, EtsTrigrams)->
    Words = string:tokens(Line," \n"),
    lists:foldl (fun (X, Acc) -> 
                         case process_word(X, EtsTrigrams) of
                             {yes, Word} ->
                                 [list_to_binary(Word) | Acc];
                             no ->
                                 Acc
                         end
                 end, [], Words).

%% @doc Remove stop words from string and stop word stems
%% @spec clean_stems(Phrase::string()) -> string()
clean (Phrase, EtsTrigrams)->
    stem_split_line(Phrase, EtsTrigrams).

edoc_clean (Lines, EtsTrigrams) ->
    FinalTerms = lists:foldl (fun ({_, _, _, Comments}, Acc) ->
                         [lists:foldl (fun (Line, Acc2) ->      
                                              case Line of
                                                  "% -*- " ++ _Terms ->
                                                      Acc2;
                                                  "%% " ++ _Terms ->
                                                      Acc2;
                                                  "% @spec " ++ _Terms ->
                                                      Acc2;
                                                  "% @end" ++ _Terms ->
                                                      Acc2;
                                                  "% @doc " ++ Terms ->
                                                      [stem_split_line (Terms, EtsTrigrams) | Acc2];
                                                  Terms ->
                                                      [stem_split_line (Terms, EtsTrigrams) | Acc2]
                                              end
                                      end, [], Comments) | Acc]
                 end, [], Lines),
    %io:format ("~n~p~n", [lists:flatten(FinalTerms)]),
    lists:flatten(FinalTerms).

%%%================================================================
%% An English stop word list from http://snowball.tartarus.org

stop("i") -> true;
stop("me") -> true;
stop("my") -> true;
stop("myself") -> true;
stop("we") -> true;
stop("us") -> true;
stop("our") -> true;
stop("ours") -> true;
stop("ourselves") -> true;
stop("you") -> true;
stop("your") -> true;
stop("yours") -> true;
stop("yourself") -> true;
stop("yourselves") -> true;
stop("he") -> true;
stop("him") -> true;
stop("his") -> true;
stop("himself") -> true;
stop("she") -> true;
stop("her") -> true;
stop("hers") -> true;
stop("herself") -> true;
stop("it") -> true;
stop("its") -> true;
stop("itself") -> true;
stop("they") -> true;
stop("them") -> true;
stop("their") -> true;
stop("theirs") -> true;
stop("themselves") -> true;
stop("what") -> true;
stop("which") -> true;
stop("who") -> true;
stop("whom") -> true;
stop("this") -> true;
stop("that") -> true;
stop("these") -> true;
stop("those") -> true;
stop("am") -> true;
stop("is") -> true;
stop("are") -> true;
stop("was") -> true;
stop("were") -> true;
stop("be") -> true;
stop("been") -> true;
stop("being") -> true;
stop("have") -> true;
stop("has") -> true;
stop("had") -> true;
stop("having") -> true;
stop("do") -> true;
stop("does") -> true;
stop("did") -> true;
stop("doing") -> true;
stop("will") -> true;
stop("would") -> true;
stop("shall") -> true;
stop("should") -> true;
stop("can") -> true;
stop("could") -> true;
stop("may") -> true;
stop("might") -> true;
stop("must") -> true;
stop("ought") -> true;
stop("a") -> true;
stop("an") -> true;
stop("the") -> true;
stop("and") -> true;
stop("but") -> true;
stop("if") -> true;
stop("or") -> true;
stop("because") -> true;
stop("as") -> true;
stop("until") -> true;
stop("while") -> true;
stop("of") -> true;
stop("at") -> true;
stop("by") -> true;
stop("for") -> true;
stop("with") -> true;
stop("about") -> true;
stop("against") -> true;
stop("between") -> true;
stop("into") -> true;
stop("through") -> true;
stop("during") -> true;
stop("before") -> true;
stop("after") -> true;
stop("above") -> true;
stop("below") -> true;
stop("to") -> true;
stop("from") -> true;
stop("up") -> true;
stop("down") -> true;
stop("in") -> true;
stop("out") -> true;
stop("on") -> true;
stop("off") -> true;
stop("over") -> true;
stop("under") -> true;
stop("again") -> true;
stop("further") -> true;
stop("then") -> true;
stop("once") -> true;
stop("here") -> true;
stop("there") -> true;
stop("when") -> true;
stop("where") -> true;
stop("why") -> true;
stop("how") -> true;
stop("all") -> true;
stop("any") -> true;
stop("both") -> true;
stop("each") -> true;
stop("few") -> true;
stop("more") -> true;
stop("most") -> true;
stop("other") -> true;
stop("some") -> true;
stop("such") -> true;
stop("no") -> true;
stop("nor") -> true;
stop("not") -> true;
stop("only") -> true;
stop("own") -> true;
stop("same") -> true;
stop("so") -> true;
stop("than") -> true;
stop("too") -> true;
stop("very") -> true;
stop("one") -> true;
stop("every") -> true;
stop("least") -> true;
stop("less") -> true;
stop("many") -> true;
stop("now") -> true;
stop("ever") -> true;
stop("never") -> true;
stop("say") -> true;
stop("says") -> true;
stop("said") -> true;
stop("also") -> true;
stop("get") -> true;
stop("go") -> true;
stop("goes") -> true;
stop("just") -> true;
stop("made") -> true;
stop("make") -> true;
stop("put") -> true;
stop("see") -> true;
stop("seen") -> true;
stop("whether") -> true;
stop("like") -> true;
stop("well") -> true;
stop("back") -> true;
stop("even") -> true;
stop("still") -> true;
stop("way") -> true;
stop("take") -> true;
stop("since") -> true;
stop("another") -> true;
stop("however") -> true;
stop("two") -> true;
stop("three") -> true;
stop("four") -> true;
stop("five") -> true;
stop("first") -> true;
stop("second") -> true;
stop("new") -> true;
stop("old") -> true;
stop("high") -> true;
stop("long") -> true;
stop(_) -> false.

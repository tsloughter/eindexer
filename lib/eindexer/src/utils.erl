%%%-------------------------------------------------------------------
%%% File    : utils.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  1 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(utils).
-export([term_frequency/4, create_idf_table/4, clean/1, generate_file_listing/1]).

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

%%% Strips all punctuation from the beginning and end of a word
strip_all (Word, []) ->
    Word;
strip_all (Word, [H | T]) ->	
    strip_all(string:strip(Word, both, H), T).
strip_all(Word) ->
    strip_all(Word, [$., $,, $;, $*]). 

stop_word_list()->[
                   "I",
                   "a",
                   "about",
                   "an",
                   "are",
                   "as",
                   "at",
                   "be",
                   "by",
                   "com",
                   "de",
                   "en",
                   "for",
                   "from",
                   "how",
                   "in",
                   "is",
                   "it",
                   "la",
                   "of",
                   "on",
                   "or",
                   "that",
                   "the",
                   "this",
                   "to",
                   "was",
                   "what",
                   "when",
                   "where",
                   "who",
                   "will",
                   "with",
                   "und",
                   "the",
                   "www"].

stop_terms(WordList,[StopWord|StopWordList])-> stop_terms(lists:delete(StopWord,WordList),StopWordList);
stop_terms(WordList,[])-> WordList.
stop_terms(WordList)-> stop_terms(WordList,stop_word_list()). 

stem_split_line(Line)->
    Words = string:tokens(Line," \n"),
    Cleaned = stop_terms(Words),
    [porter:stem(string:to_lower(strip_all(Word))) || Word <- Cleaned].

%% @doc Remove stop words from string and stop word stems
%% @spec clean_stems(Phrase::string()) -> string()
clean (Phrase)->
     WordList=stem_split_line(Phrase),
     stop_terms(WordList).
     %string:join(Clean," ").

%split_line(Line)->
%    Words = string:tokens(Line," \n"),
%    Cleaned = stop_terms(Words),
%    [ Word   || Word <- Cleaned].
%

%% @doc Remove stop words from string
%% @spec clean(Phrase::string()) -> string()
%clean(Phrase)->
%     WordList=split_line(Phrase),
%     stop_terms(WordList).

%     string:join(Clean," ").

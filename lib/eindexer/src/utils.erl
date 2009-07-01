%%%-------------------------------------------------------------------
%%% File    : utils.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  1 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(utils).
-export([clean/1, generate_file_listing/1]).

generate_file_listing(Dir) ->
    {ok, FileListing} = file:list_dir(Dir),
    FileListing.

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

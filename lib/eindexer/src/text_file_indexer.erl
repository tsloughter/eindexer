%%%-------------------------------------------------------------------
%%% File    : text_file_parser.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  1 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(text_file_indexer).
-export([index/6]).

index (Dir, Docs, DocTermTable, TermsTable, IDFTable, EtsTrigram) ->
    Files = utils:generate_file_listing(Dir),    
    NumDocs = lists:foldl (fun (AbsolutePath, Acc) ->
                                   ets:insert (Docs, {AbsolutePath, AbsolutePath}),
                                   case file:read_file(AbsolutePath) of
                                       {ok, Binary} ->
                                           insert_words(binary_to_list(Binary), AbsolutePath, TermsTable, DocTermTable, EtsTrigram),
                                           Acc+1;
                                       {error, _Reason} ->
                                           Acc
                                   end
                           end, 0, Files),
 
    utils:create_idf_table (NumDocs, TermsTable, DocTermTable, IDFTable).

insert_words (File, Entry, TermTable, EntryTermTable, EtsTrigram) ->
    Words = utils:clean (File, EtsTrigram),    
    lists:foreach (fun (Word) ->
                           utils:term_frequency (Entry, Word, TermTable, EntryTermTable)
                   end, Words).

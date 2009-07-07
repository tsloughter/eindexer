%%%-------------------------------------------------------------------
%%% File    : text_file_parser.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  1 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(edoc_indexer).
-export([index/6]).

index (Dir, Docs, DocTermTable, TermsTable, IDFTable, EtsTrigram) ->
    Apps = utils:app_generate_file_listing(Dir),
    NumDocs = lists:foldl (fun (App, Acc) ->
                                   {ok, [Terms]} = file:consult(App),
                                   Name = element(2, Terms),
                                   List = element(3, Terms),
                                   {description, Description} = lists:nth(1, List),
                                   {vsn, Version} = lists:nth(2, List),
                                   text_insert_words(Description, Name, TermsTable, DocTermTable, EtsTrigram),
                                   Tokens = string:tokens(App, "/"),                                
                                   Path = "/"++filename:join(lists:sublist(Tokens, length(Tokens)-2)),
                                   case utils:get_overview_file(Path) of
                                       [] ->
                                           no_overview;
                                       [Overview] ->
                                           {ok, Binary} = file:read_file(Overview),
                                           text_insert_words(binary_to_list(Binary), Name, TermsTable, DocTermTable, EtsTrigram)
                                   end,
                                   
                                   Files = utils:edoc_generate_file_listing(Path),
                                   ets:insert (Docs, {application, Name, Description, Version, App}),
                                   lists:map (fun (AbsolutePath) ->                    
                                                      Lines = edoc:read_comments(AbsolutePath),
                                                      edoc_insert_words(Lines, Name, TermsTable, DocTermTable, EtsTrigram)
                                              end, Files),
                                   Acc+1         
                           end, 0, Apps),
    
    utils:create_idf_table (NumDocs, TermsTable, DocTermTable, IDFTable).

edoc_insert_words (Lines, Entry, TermTable, EntryTermTable, EtsTrigram) ->
    Words = utils:edoc_clean (Lines, EtsTrigram),    
    lists:foreach (fun (Word) ->
                           utils:term_frequency (Entry, Word, TermTable, EntryTermTable)
                   end, Words).

text_insert_words (Description, Entry, TermTable, EntryTermTable, EtsTrigram) ->
    Words = utils:clean (Description, EtsTrigram),    
    lists:foreach (fun (Word) ->
                           utils:term_frequency (Entry, Word, TermTable, EntryTermTable)
                   end, Words).

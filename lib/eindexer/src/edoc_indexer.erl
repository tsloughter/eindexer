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
                                   io:format ("~nApp: ~p~n", [App]),
                                   {ok, [Terms]} = file:consult(App),
                                   Name = element(2, Terms),
                                   List = element(3, Terms),
                                   {description, Description} = lists:nth(1, List),
                                   {vsn, Version} = lists:nth(2, List),
                                   Tokens = string:tokens(App, "/"),                                
                                   Path = "/"++filename:join(lists:sublist(Tokens, length(Tokens)-2)),
                                   io:format ("~nPath: ~p~n", [Path]),
                                   Files = utils:edoc_generate_file_listing(Path),
                                   ets:insert (Docs, {application, Name, Description, Version, App}),
                                   lists:map (fun (AbsolutePath) ->                    
                                                      Lines = edoc:read_comments(AbsolutePath),
                                                      insert_words(Lines, AbsolutePath, TermsTable, DocTermTable, EtsTrigram)
                                              end, Files),
                                   Acc+1         
                           end, 0, Apps),
    
    utils:create_idf_table (NumDocs, TermsTable, DocTermTable, IDFTable).

insert_words (Lines, Entry, TermTable, EntryTermTable, EtsTrigram) ->
    Words = utils:edoc_clean (Lines, EtsTrigram),    
    lists:foreach (fun (Word) ->
                           utils:term_frequency (Entry, Word, TermTable, EntryTermTable)
                   end, Words).

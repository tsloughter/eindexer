%% This is the application resource file (.app file) for the eindexer,
%% application.
{application, eindexer, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [eindexer_app,
              eindexer_sup,
              eindexer,
              
              indexer_trigrams,
              conditions,
              porter,
              text_file_indexer,
              utils]},
   {registered,[eindexer_sup]},
   {applications, [kernel, stdlib]},
   {mod, {eindexer_app,[]}},
   {start_phases, []}]}.


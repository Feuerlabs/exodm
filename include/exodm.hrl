
-compile({parse_transform, lager_transform}).

%% Lager logging levels
%%   debug, info, notice, warning, error, critical, alert, emergency, none.


-define(debug(Fmt), lager:debug(Fmt)).
-define(debug(Fmt, Args), lager:debug(Fmt, Args)).
-define(debug(Attrs, Fmt, Args), lager:debug(Attrs, Fmt, Args)).

-define(warning(Fmt), lager:warning(Fmt)).
-define(warning(Fmt, Args), lager:warning(Fmt, Args)).
-define(warning(Attrs, Fmt, Args), lager:warning(Attrs, Fmt, Args)).

-define(error(Fmt), lager:error(Fmt)).
-define(error(Fmt, Args), lager:error(Fmt, Args)).
-define(error(Attrs, Fmt, Args), lager:error(Attrs, Fmt, Args)).

-define(critical(Fmt), lager:critical(Fmt)).
-define(critical(Fmt, Args), lager:critical(Fmt, Args)).
-define(critical(Attrs, Fmt, Args), lager:critical(Attrs, Fmt, Args)).


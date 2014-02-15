-module(clock_vdir).

-export([arg_rewrite/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("yaws/include/yaws.hrl").

-export([join/2]).

join(List, Sep) ->
    lists:foldl(fun(A, "") -> A; (A, Acc) -> Acc ++ Sep ++ A
                end, "", List).


arg_rewrite(ARG) ->
    Req = ARG#arg.req,
    io:fwrite("----->rewrite_mod for request: ~p\n",[ARG#arg.req]),
    io:fwrite("opaque: ~p\n",[ARG#arg.opaque]),

    case Req#http_request.path of
        {abs_path, RawPath} ->
            case (catch yaws_api:url_decode_q_split(RawPath)) of
                {'EXIT', _} ->
                    %%broken request - ignore let yaws_server handle it.
                    ARG2 = ARG;
                {"", _QueryPart} ->
                    ARG2 = ARG;
                {"/", _QueryPart} ->
                    %%don't allow vdir to be specified for root -
                    %% it doesn't make sense
                    ARG2 = ARG;
                {DecPath, _QueryPart} ->
                    SC = get(sc),

                    io:fwrite("{DecPath, _QueryPart}: ~p\n",[{DecPath, _QueryPart}]),

                    %%vdirpath/3 will return the longest(ie most specific)
                    %% 'virtual directory' match for our request
                    %%It retrieves the vdir definitions from #arg.opaque
                    case vdirpath(SC, ARG, DecPath) of
                        {"",_MainDocRoot} ->
                            %%no virtual dir corresponding to this
                            %% http_request.path
                            io:fwrite("No vdir: \n"),
                            ARG2 = ARG;
                        {Virt,DocRoot} ->
                            io:fwrite("{Virt,DocRoot}: ~p\n",[{Virt,DocRoot}]),

                            %%the virtual-path of our request matches a
                            %% vdir specification
                            %% - rewrite ARG accordingly.

                            ARG2 = ARG#arg{docroot = DocRoot,
                                           docroot_mount = Virt}
                    end
            end;
        _Else ->
            ARG2 = ARG
    end,

    ARG2.


vdirpath(SC, ARG, RequestPath) ->
    Opaquelist = ARG#arg.opaque,
    %% !todo - move out of opaque.
    %%  We don't want to scan all opaque entries each time
    %%  - vdir directives should be pre-collated into a list somewhere.
    %%  (own field in sconf record)


    RequestSegs = string:tokens(RequestPath,"/"),
    f(["RequestSegs",RequestSegs]),


    %% Accumulator is of form {RequestSegs,{VdirMountPoint,VdirPhysicalPath}}
    Matched =
        lists:foldl(
          fun(ListItem,Acc) ->
                  case ListItem of
                      {"vdir",Vmap} ->

                          {ReqSegs,VdirSpec} = Acc,

                          [Virt |PhysParts] = string:tokens(Vmap," \t"),
                          VirtSegs = string:tokens(Virt,"/"),
                          case lists:prefix(VirtSegs,ReqSegs) of
                              true ->
                                  {LongestSoFar,_} = VdirSpec,
                                  if length(Virt) > length(LongestSoFar) ->
                                          %% reassemble (because physical
                                          %% path may have spaces)
                                          Phys = yaws:join_sep(PhysParts, " "),

                                          {ReqSegs, {Virt, Phys}};
                                     true ->
                                          Acc
                                  end;
                              false ->
                                  Acc
                          end;
                      _Else ->
                          %% irrelevant member of opaque list. no change in
                          %% accumulator
                          Acc
                  end
          end, {RequestSegs,{"",""}}, Opaquelist),


    case Matched of
        {_RequestSegs, {"",""}} ->
            %% no virtual dir corresponding to this http_request.path
            %% NOTE - we *don't* know that the state of ARG#arg.docroot
            %% currently reflects the main docroot
            %% specified for the virtual server in the conf file.
            %% This is because we may be being called from a page that is
            %% under a vdir, and so docroot may
            %% have been rewritten. It may also have been rewritten by an
            %% appmod or arg_rewrite_mod.
            %% Therefore we need to get it directly from the sconf record.

            Result = {"",SC#sconf.docroot};
        {_RequestSegs, {Virt,DocRoot }} ->
            %%sanitize Virt & DocRoot so that they are correct with
            %% regards to leading & trailing slashes
            case string:right(Virt,1) of
                "/" ->
                    VirtualDir = Virt;
                _ ->
                    VirtualDir = Virt ++ "/"
            end,
            DR = string:strip(DocRoot,right,$/),

            Result = {VirtualDir, DR}
    end,

    %% return {VdirURI, Physpath}  - i.e tuple representing the data
    %% specified in conf file for the 'vdir' directive.
    Result.



f(Args) ->
    io:format(lists:append(["~p~n" || _ <- Args]),Args).

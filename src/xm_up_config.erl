%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 十月 2017 18:02
%%%-------------------------------------------------------------------
-module(xm_up_config).
-author("pingjianwei").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, ?MODULE).

-record(state, {mer_list_map
  , public_key
  , success_resp_code_list
  , fail_resp_code_list
  , resp_code_mapping
  , mer_router_map}).

%%%===================================================================
%%% API
%%%===================================================================
-export([get_config/1,
  get_mer_prop/2,
  get_code_mapping/1,
  get_mer_id/1]).

get_mer_prop(MerId, Key) when is_binary(MerId) ->
  get_mer_prop(binary_to_atom(MerId, utf8), Key);
get_mer_prop(MerId, Key) when is_atom(MerId) ->
  gen_server:call(?SERVER, {get_mer_prop, MerId, Key}).

get_mer_id(PaymentType) when is_atom(PaymentType) ->
  gen_server:call(?SERVER, {get_mer_id, PaymentType}).

get_config(Key) when is_atom(Key) ->
  gen_server:call(?SERVER, {xm_up_config, Key}).

get_code_mapping(Key) when is_atom(Key) ->
  get_code_mapping(atom_to_binary(Key, utf8));
get_code_mapping(Key) when is_binary(Key) ->
  gen_server:call(?SERVER, {resp_code_mapping, Key}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  State = #state{
    mer_list_map = get_mer_list()
    , mer_router_map = get_route()
    , resp_code_mapping = getRespCodeMapping()
    , public_key = get_xm_up_public_key()
  },
  lager:debug("~p get env config = ~p", [?SERVER, State]),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_mer_prop, MerId, Key}, _From, #state{mer_list_map = MerListMap} = State)
  when is_atom(Key) ->
  MerPropsMap = maps:get(MerId, MerListMap),
  Value = maps:get(Key, MerPropsMap, undefined),
  {reply, Value, State};

handle_call({get_mer_id, PaymentType}, _From, #state{mer_router_map = MerRouteMap} = State)
  when is_atom(PaymentType) ->
  {_, MerList} = maps:get(PaymentType, MerRouteMap),
  UpMerId = lists:nth(rand:uniform(length(MerList)), MerList),
  {reply, UpMerId, State};

handle_call({resp_code_mapping, Key}, _From, #state{resp_code_mapping = RespCodeMapping} = State)
  when is_binary(Key) ->
  Code = maps:get(Key, RespCodeMapping),
  {reply, Code, State};

handle_call({xm_up_config, Key}, _From, State) ->
  Return = do_get_config(Key, State),
  {reply, Return, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_get_config(public_key, #state{public_key = PublicKey} = State) when is_record(State, state) ->
  PublicKey;
do_get_config(Key, _) when is_atom(Key) ->
  {ok, Value} = application:get_env(Key),
  Value.
%%------------------------------------------------------------------------------

get_keys_dir_config() ->
  {ok, UpKeysDirConfig} = application:get_env(?APP, xm_up_keys_dir),
  UpKeysDirConfig.

get_xm_up_public_key() ->
  PublicKeyFileName = xfutils:get_filename(?APP,get_keys_dir_config() ++ [xm_up_public_key_file]),
  lager:debug("PublicKeyFileName = ~p", [PublicKeyFileName]),
  try
    {ok, PemBin} = file:read_file(PublicKeyFileName),
    [Certificate] = public_key:pem_decode(PemBin),
    %{_, DerCert, _} = Certificate,
    %Decoded = public_key:pkix_decode_cert(DerCert, otp),
    %PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    PublicKey = public_key:pem_entry_decode(Certificate),
    lager:debug("public key = ~p", [PublicKey]),
    PublicKey
  catch
    error:X ->
      lager:error("read public key file ~p error! Msg = ~p", [PublicKeyFileName, X]),
      {<<>>, <<>>}

  end.


%%--------------------------------------------------------------------

get_mer_list() ->
  {ok, MerPropsMap} = application:get_env(?APP, xm_up_mer_props),
  lager:debug("maps origal = ~p", [MerPropsMap]),
  F = fun
        (MerId, PropMap) when is_atom(MerId), is_map(PropMap) ->
          lager:debug("MerId = ~p,maps origal = ~p", [MerId, PropMap]),
          PrivateKey = load_private_key(MerId),
          PublicKey = load_public_key(MerId),
          MapsRet1 = maps:put(privateKey, PrivateKey, PropMap),
          MapsRet = maps:put(publicKey, PublicKey, MapsRet1),
          MapsRet
      end,
  MerPropsMapWithPK = maps:map(F, MerPropsMap),
  MerPropsMapWithPK.

key_file_name(MerId, Type)
  when is_atom(MerId), is_atom(Type)
  , ((Type =:= private) or (Type =:= public)) ->
  MerIdBin = atom_to_binary(MerId, utf8),
  lager:debug("keys derictor:~p", [get_keys_dir_config()]),
  KeyPath = xfutils:get_path(?APP,get_keys_dir_config()),
  Ext = case Type of
          private ->
            ".key";
          public ->
            ".pub"
        end,
  KeyFileName = list_to_binary([KeyPath, MerIdBin, Ext]),
  KeyFileName.

load_private_key(MerId) when is_atom(MerId) ->
  KeyFileName = key_file_name(MerId, private),
  lager:debug("private key file name = ~p", [KeyFileName]),
  {ok, Pwd} = application:get_env(?APP, xm_private_key_default_pwd),
  PrivateKey = try
                 xfutils:load_private_key(KeyFileName, Pwd)
               catch
                 error:noent ->
                   lager:error("Could not file file ~p", [KeyFileName]),
                   <<>>;
                 _:_ ->
                   lager:error("load private key from ~p failed!set it to <<>>", [KeyFileName]),

                   <<>>
               end,
  PrivateKey.

load_public_key(MerId) when is_atom(MerId) ->
  KeyFileName = key_file_name(MerId, public),
  lager:debug("public key file name = ~p", [KeyFileName]),
  PublicKey = try
                xfutils:load_public_key(KeyFileName, rsa)
              catch
                _:_ ->
                  lager:error("load public key from ~p failed!set it to <<>>", [KeyFileName]),
                  <<>>
              end,
  PublicKey.

%%--------------------------------------------------------------------

get_route() ->
  {ok, UpMerList} = application:get_env(?APP, xm_up_mer_list),
  maps:from_list(UpMerList).

getRespCodeMapping() ->
  {ok, RespCodeMapping} = application:get_env(?APP, resp_code_mapping),
  RespCodeMapping.

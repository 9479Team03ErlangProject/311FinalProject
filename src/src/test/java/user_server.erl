%%%-------------------------------------------------------------------
%%% @author Christian Lucina
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module provides a simple in-memory user authentication system
%%%  using Erlang Term Storage (ETS). It includes functionalities for
%%%  starting and stopping the server, registering users, and authenticating them.
%%% @end
%%% Created : 01. Dec 2024 5:26 pm
%%%-------------------------------------------------------------------

-module(user_server). % Declares the name of the module as 'user_server'.
-author("Christian Lucina"). % Declares the author's name.
-export([start/0, register_user/2, authenticate_user/2, stop/0]).
% Exports the functions start/0, register_user/2, authenticate_user/2, and stop/0
% to make them accessible outside this module.

% Starts the user server by creating an ETS table named 'users'.
start() ->
  ets:new(users, [named_table, public]),
  % Creates an ETS table named 'users', which is accessible by its name (named_table)
  % and publicly accessible (public).
  {ok, "User server started"}. % Returns a success tuple indicating the server has started.

% Stops the user server by deleting the ETS table named 'users'.
stop() ->
  ets:delete(users),
  % Deletes the ETS table named 'users', removing all user data.
  {ok, "User server stopped"}. % Returns a success tuple indicating the server has stopped.

% Registers a user with a given Username and Password.
register_user(Username, Password) ->
  case ets:lookup(users, Username) of
    % Checks if the Username already exists in the 'users' ETS table.
    [] ->
      % If no record is found, the user does not exist.
      ets:insert(users, {Username, Password}),
      % Inserts the Username and Password as a tuple into the 'users' ETS table.
      {ok, "User registered"};
    % Returns a success tuple indicating the user has been registered.
    _ ->
      % If a record is found, the user already exists.
      {error, "User already exists"}
    % Returns an error tuple indicating the user registration failed.
  end.

% Authenticates a user by checking the given Username and Password.
authenticate_user(Username, Password) ->
  case ets:lookup(users, Username) of
    % Looks up the Username in the 'users' ETS table.
    [{_, StoredPassword}] when StoredPassword == Password ->
      % If the Username exists and the Password matches the stored password.
      {ok, "Authenticated"};
    % Returns a success tuple indicating successful authentication.
    _ ->
      % If the Username does not exist or the Password is incorrect.
      {error, "Invalid credentials"}
    % Returns an error tuple indicating authentication failed.
  end.
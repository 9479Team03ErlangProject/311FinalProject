%%%-------------------------------------------------------------------
%%% @author Christian Lucina
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module implements a high-level interface for a chat application.
%%%  It integrates user authentication and chat room functionalities.
%%% @end
%%% Created : 01. Dec 2024 5:27 pm
%%%-------------------------------------------------------------------

-module(chat_app). % Declares the module name as 'chat_app'.
-author("Christian Lucina"). % Declares the author's name.
-export([start/0, register_user/2, login/2, join_room/2, send_message/3]).
% Exports the functions start/0, register_user/2, login/2, join_room/2, and send_message/3
% so they can be accessed outside this module.

% Starts the chat application by initializing the user server.
start() ->
  user_server:start(),
  % Calls the start/0 function in the user_server module to initialize user storage.
  io:format("Chat application started.~n").
% Prints a message indicating the chat application has started.

% Registers a new user by delegating the request to the user_server module.
register_user(Username, Password) ->
  user_server:register_user(Username, Password).
% Calls the register_user/2 function in the user_server module.

% Authenticates a user using the user_server module.
login(Username, Password) ->
  case user_server:authenticate_user(Username, Password) of
    % Calls authenticate_user/2 in the user_server module and checks the result.
    {ok, _} ->
      % If authentication succeeds:
      io:format("~s logged in successfully.~n", [Username]),
      % Prints a success message for the user.
      {ok, Username};
    % Returns a tuple indicating successful login with the username.
    {error, Reason} ->
      % If authentication fails:
      io:format("Login failed: ~s~n", [Reason]),
      % Prints an error message with the failure reason.
      {error, Reason}
    % Returns an error tuple with the reason for failure.
  end.

% Allows a user to join a chat room.
join_room(Username, RoomName) ->
  chat_room:start(RoomName),
  % Starts the chat room process by calling start/1 in the chat_room module.
  chat_room:join(RoomName, Username).
% Sends a join request for the user to the chat room.

% Sends a message from a user to a chat room.
send_message(Username, RoomName, Message) ->
  chat_room:send_message(RoomName, {Username, Message}).
% Sends a message to the specified room using send_message/3 from the chat_room module.
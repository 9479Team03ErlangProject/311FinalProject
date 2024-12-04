%%%-------------------------------------------------------------------
%%% @author Christian Lucina
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  This module implements a simple chat room functionality. It allows users
%%%  to join or leave a room, send messages, and handle crashes with recovery.
%%% @end
%%% Created : 01. Dec 2024 5:27 pm
%%%-------------------------------------------------------------------

-module(chat_room).
-author("Christian Lucina").
-export([start/1, join/2, leave/2, send_message/2, crash_recovery/1]).

% Supervisor initialization for automatic crash recovery.
start(RoomName) ->
  process_flag(trap_exit, true),
  RoomAtom = list_to_atom(RoomName),
  register(RoomAtom, spawn(fun() -> loop(RoomAtom, []) end)).

% Chat room loop that handles user join, leave, message, and crashes.
loop(RoomName, Members) ->
  receive
    {join, User} ->
      io:format("~s joined ~s~n", [User, RoomName]),
      loop(RoomName, [User | Members]);

    {leave, User} ->
      io:format("~s left ~s~n", [User, RoomName]),
      loop(RoomName, lists:delete(User, Members));

    {message, User, Message} ->
      lists:foreach(fun(Member) ->
        if Member /= User ->
          io:format("Message to ~s from ~s: ~s~n", [Member, User, Message])
        end
                    end, Members),
      loop(RoomName, Members);

    {'EXIT', _Pid, _Reason} ->
      crash_recovery(RoomName)
  end.

% Handle joining a room.
join(RoomName, User) ->
  RoomAtom = list_to_atom(RoomName),
  RoomAtom ! {join, User}.

% Handle leaving a room.
leave(RoomName, User) ->
  RoomAtom = list_to_atom(RoomName),
  RoomAtom ! {leave, User}.

% Handle sending a message.
send_message(RoomName, {User, Message}) ->
  RoomAtom = list_to_atom(RoomName),
  RoomAtom ! {message, User, Message}.

% Handle chat room crash recovery.
crash_recovery(RoomName) ->
  io:format("Recovering chat room ~s~n", [RoomName]),
  start(RoomName).


-module(name).
-export([foo/1]).

% lot of code here ...

% case 1: is it public?
% case 2: you want to make it public to test it
% case 3: you want to make it private
% case 4: change the name of the function

% go to the top
% check if it's exported
% write the name (again) and write the arity
% to rename: change the name in multiple places
% come back here again

foo(<pattern1>) ->
    <body1>;
foo(<pattern2>) ->
    <body2>;
foo(<pattern3>) ->
    <body3>.


# habit-builder

This is just small tool to track repetitions of some action.
The idea is we have start time and credits. Each credit allow to do something very interesting, like to use laptop of play games and so on, for one day (for example). Each `n` hours we are removing one credit but there is the way to add credit by doing something good (to read aloud one paragraph, for example).
When amount of credits more than `0` he can have access to games. In case `0` or less.. Well.. Sorry...
So I'm not sure if my brain can remember how many credits we still have. And this really simple thing allows to keep track of this.

To start:
create file `habit` or any other file with name like `reading`.
In this file add:
line 1: time when you start tracking in UTC format. Like `2024-04-10 00:00:00 UTC`.
line 2: amount of hours when `1` credit is removed
line 3: amount of credits
Check `./habit-example` for example.

Build:
```
nix build
```

Run:
```
./result/bin/habit-builder-exe view <filename>
```
to view current credits

```
./result/bin/habit-builder-exe add <filename>
```
to add one credit

```
./result/bin/habit-builder-exe remove <filename>
```
to remove one credit

In case file name is habit you can skip it.

## Additional idea to Check
Also I wanted to check how Haskell can live with clean architecture and on this very primitive example I love how it works.

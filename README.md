# Haskell stack project for (my) Advent of Code solutions

Requires lynx to be installed for displaying html pages in the terminal. <br>
So probably only works on linux unless you modify the "showHtml" function.

You must also have your AoC session key cookie in a file called "session-key". My session key is not included in the repository :(

src/Solutions.hs collects different year's solutions to a single module <br>
app/Main.hs has helpers to communicate with AoC web API (using a third party library)

Run "stack run year day" to run the solving procedure for that year and day,
ex. "stack run 2019 1" to solve the first day puzzles in 2019.

Assuming the solutions can be found through the Solutions module(s), it will download and cache your input,
display puzzle prompts (the coding task) and use the solutions to submit answers automatically.
Feel free to use the code to submit your own answers, but since this repo is meant to store my own answers, they will be included as _free samples_ (unless there are complaints). So you may want to liberally press delete in the src/ folder after cloning.

You can use "undefined" solutions causing the program to <s>crash</s> _exit gracefully_ to incrementally solve a puzzle.
So on first run it will get the input, display part 1 description and _exit_.
After creating the part 1 solution it will upload that and either print a response for incorrectness of the solution
or that it is correct, followed by the second prompt and _exit_.

Internal cache of the Advent module and your solutions outputs and server responses on succssful attempts will be stored under /solutionstemp. A .success file (ex. Part1.success) will be created with the server response for each correct answer. If for some reason the server connection is interrupted during submission, but the submitted answer is correct, you may receive a helpful "Invalid" from the submission process making it impossible to determine the status of the part. You may check from the website whether the solution was accepted (or assume it was) and create an empty Partx.success to tell the program to skip submission.

AoC API calls take a while, don't panic. That said the solution calculation might leak space spuriously or diverge because Haskell, so there is a timeout on that part, defaulting to 3 seconds (not sure if this even helps with the leaks). To make it easier to determine if the program is stuck in the solving process, rather than waiting for the server, it will print the following when calculating:
```
No solution yet, calculating...
Calculated, submitting...
```
So the second line should appear in 3 seconds or there should be a timeout. Any other "hangs" are due to AoC API connections.
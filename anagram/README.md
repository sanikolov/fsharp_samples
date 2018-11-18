This is a hard combinatorial puzzle by Trustpilot, implemented in dotnet Core.
http://followthewhiterabbit.trustpilot.com/cs/step3.html

>You've made an important decision. Now, let's get to the matter.
>
>We have a message for you. But we hid it. 
>Unless you know the secret phrase, it will remain hidden.
>
>Can you write the algorithm to find it?
>
>Here is a couple of important hints to help you out:
>- An anagram of the phrase is: "poultry outwits ants"
>- There are three levels of difficulty to try your skills with
>- The MD5 hash of the easiest secret phrase is "e4820b45d2277f3844eac66c903e84be"
>- The MD5 hash of the more difficult secret phrase is "23170acc097c24edb98fc5488ab033fe"
>- The MD5 hash of the hard secret phrase is "665e5bcb0c20062fe8abaaf4628bb154"
>Here is a list of english words, it should help you out.
>
>Type the secret phrase here to see if you found the right one  



Filters in this solution have been hand tuned to the dataset.
The general idea is to constrain the word lengths in descent 0, then 1, then 2,
otherwise the combinatorial explosion is too large. No more than 3 descents are allowed.
Subsequent to finding the right phrases I constrained the descents even more to get the answers faster.
It takes less than 2 minutes on modern CPUs.

An alternative to this approach is to construct all permutations of the original phrase and then
try to find delineation points between words and insert spaces and apostrophe chars in between
but the combinatorial space is too large (18! / (2!\*4!\*2!\*2!)). So I did not pursue that track.
Finding the "hard secret phrase" remains elusive. It's unclear how to prune recursion better.
Good luck writing a readable, concise solution to this problem in C++.

# nomebot
A Markov-chain IRC Chatbot in Haskell

NomeBot creates semi-realistic text using Markov chains to create a sqlite database from a given training corpus. NomeBot
is intended to maintain the 'flavor' of its training text, and thus does not learn from its conversations while in chat
mode. The default chain length is 2, but can be changed by modifying 'chainLength' in Trainer.hs.

Running ./NomeBot -h will disply the following command line options:
```
-t           --train          Put bot in training mode 
-s SERVER    --server=SERVER  Specify server
-p NUMBER    --port=NUMBER    Specify port
-n NICK      --nick=NICK      Specify nick
-c #CHANNEL  --chan=#CHANNEL  Specify channel
-d FILE      --database=FILE  Specify database
```
Requires a small change to run on real irc networks, currently `reply` in Chat.hs only ignores messages that begin with 
':irc', which is not the naming convention for all network messages. Thus, NomeBot may reply to all server messages, flooding
the chat server and causing it to get kicked.

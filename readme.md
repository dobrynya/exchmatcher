## Buiding and testing

To be able to run tests you should write the following
```
./gradlew test
```
All required binaries and dependencies will be loaded automatically.

After tests succeed you can discover file `result.txt` containing all clients' portfolios 
after orders from `orders.txt` have been processed in the current directory. 

## Idea and some details

The main idea behind this implementation is to have an Exchange instance processing orders and 
spawning new Exchange instances instead of mutating the only instance. This case is able
to be scaled horizontally as well as vertically. In real life the same idea may be used to process orders 
operating on streams, f.e. as a Flow in Akka Streams. 

There are the following entities:
* Client for storing client portfolio with its parameters, name and securities
* Order for representing a bid and an ask
* Exchange for holding clients' portfolios and processing orders
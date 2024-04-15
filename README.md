# wheresCroc
An R script with a function that can be passed to the runWheresCroc function.
You are a ranger in Wollomunga national park, in outback Australia. Crocodiles in this park have sensors attached that say where they are and the water conditions (salinity, phosphate and nitrogen levels) of the water the crocodile is current swimming in. The park consists of 40 waterholes, each of which is reachable only from its neighbors. The park has records of the calcium, salinity and alkalinity distributions in each waterhole. The sensor on one crocodile, 'Croc', has broken. 
It no longer says where the croc is, however, it does still provide water condition readings. You need to find Croc. 
There are also two Swedish backpackers in the park, wandering around at random, visiting waterholes. If they end up in the same waterhole as Croc, they will be eaten. You can move a ranger around the waterhole network and search for Croc at different locations. 
Your score is the number of moves it takes to find Croc. Your task is to implement a control system to compete in the Where’s Croc game. 
You will use hidden Markov models and associated algorithms to work out where Croc is given the sequence of observable variables given to you in the game. R

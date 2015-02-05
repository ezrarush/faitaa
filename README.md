#Description

A modern OpenGL networked fighter game

#How to Run

##SBCL on Windows

Install SBCL and Quicklisp (https://www.youtube.com/watch?v=VnWVu8VVDbI)

Download this repository and place it in your quicklisp\local-projects\ folder so that quicklisp can find it.  

Download my [network-engine](https://github.com/ezrarush/network-engine) repository and place it in your quicklisp\local-projects\ folder so that quicklisp can load it as a dependency.

Download my [graphics-engine](https://github.com/ezrarush/graphics-engine) repository and place it in your quicklisp\local-projects\ folder so that quicklisp can load it as a dependency. 

###Server

Run the following in the command line from the project folder:

```
sbcl --load run-server.lisp
```

###Player One

Run the following in a second command line instance from the project folder:

```
sbcl --load run-client-one.lisp
```
###Player Two

Run the following in a third command line instance from the project folder:

```
sbcl --load run-client-two.lisp
```

#References

[Online multiplayer proof-of-concept](https://github.com/andras-szabo/online-multiplayer-proof-of-concept)

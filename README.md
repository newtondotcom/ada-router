# ada-router
This is an ADA project designed to act as a router with a cache. The data structure used is a linked list of associative data. 
Another part of the project that was not completed is the representation of the cache as a prefix cache. 
We implemented many command line options for the operation of this program.
You can find a lot of documentations in french that me and [Mathieu](https://github.com/mathieuzeidler) realized inside this repo.

For those who don't know, a router with cache is a networking device that uses memory to store frequently accessed data. This allows the router to quickly retrieve the data from cache instead of having to retrieve it from its original source, reducing access time and improving network performance. The cache is typically used to store information such as web pages, DNS lookups, and routing tables. This can help to speed up internet browsing and other network-dependent tasks, by allowing the router to respond more quickly to requests.

routeurLL.adb is the main file.
cli.adb is the module taking care of command line and text conversion.
tr_liste.adb is the module implementing the associative chained list in which the routing table and the cache are stored. 



This project was realized during my engineering studies in Digital Sciences at ENSEEIHT.

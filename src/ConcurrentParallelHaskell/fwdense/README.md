
Floyd-Warshall Algorithm is an algorithm for finding the shortest path between
all the pairs of vertices in a weighted graph. This algorithm works for both
the directed and undirected weighted graphs. But it does not work for the
graphs with negative cycles (where the sum of the edges in a cycle is
negative).

A weighted graph jis a graph in which edge has a numerical value associated
with it.

Floyd-Warshall Algorithm follows the dynamic programming approach to find the
shortest path.

-----------------------


Solving this algorithm might look something like this ...

You can think of the algorithm like this: shortestPath g i j k gives the length
of the shortest path between i and j, passing through the vertices up to k
only. At k == 0, the path between the two vertices is a direct edge. For a
non-zero k, there are 2 cases:
(a) either the shortest path from i to j passes through k 
(b) or it does not.
Then the shortest path is the minimum of these 2 scenarios

Let's start along the path by representing the vertices and we can numbered
them from one, two ... etc and we have a function `weight g i j` that gives the
weight of the edge from i to j in the graph g, the algorithm can be described
by this pseudo-code:
```haskell
shortestPath :: Graph -> Vertext -> Vertex -> Vertex -> Weight
shortestPath g i j 0 = weight g i j
shortestPath g i j k = min (shortestPath g i j (k-1))
                           (shortestPath g i k (k -1) + shortestPath g k j (k - 1))
```



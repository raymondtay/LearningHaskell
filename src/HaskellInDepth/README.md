
#### Package dependencies and the related problems

You may encounter many discussion on the Internet suggesting that one of the
biggest difficulties with practical usage of Haskell lies in dependency
management. You rely on some packages when you develop your own project: we say
that your project depends on them. Those packages depend on other packages,
thus creating a dependency graph. If you explore the dependency graph created
for the stock quotes project you will see that there are more than 70 Haskell
packages that this project depends on either directly or indirectly.



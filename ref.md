---
layout: default
title: Ref
---

# Ref

<p class="lead">
`Ref` is the heart of handy that makes everything else rather easy.
</p>


## Ref is a monad

For the moment, don't worry about the "monad" word.  There are lots of complicated explanations out there, but it's very simple:

If you have a `Ref[T]`, then you can say `ref.map(...)` or `ref.flatMap(...)` to get a `Ref[Something else]`.  You use `map` if your method is of type `(T) -> SomethingElse` and `flatMap` if your method is of type `(T) -> Ref[SomethingElse]`.  

Scala has some syntactic sugar that means you can also write

    for (
    	item <- ref;
    	other <- doSomethingWith(item);
    	yetAnother <- doSomethingElseWith(yetAnother)
    ) yield yetAnother

and you'll end up with a `Ref[yetAnother]`.

The upshot of this is:

- regardless of *what we put in the `Ref`*, `Ref` adds its functionality around it
- we can be sure that if we start with a `Ref`, we'll still have a `Ref` at the end of our algorithm.  

That's going to make things very easy, because at the end of our algorithm we'll add some implicit conversions: for instance to turn a `Ref[Something]` into an HTTP response containing JSON markup.  And that includes doing automatic error handling.


## Ref is an *ad-hoc* Monad

Most monads are a specific class: for example: `Future`, `Option`, `Try`.  That means that if your pipeline starts with an `Option`, you can be sure your outermost type will be a `Option` at the end.  This is all well and good if your whole algorithm uses the same monad types all the way through.  But suppose you want to swap from a synchronous database driver that uses `Try` to an asynchronous one that uses `Future` -- the types in your algorithm change and you need to recompile the program, not just configure a different database driver.

Ref is different.  It is a trait.

This means that there can be many different classes that implement `Ref`, so long as they meet the contract.  

This means we can define on `Ref` the functionality we need at the end.  Then, our algorithm can call whatever it likes, and the intermediate steps can use whatever subtype of Ref they like.  But we're still guaranteed at the end of our algorithm to have a type that has the functionality we need.

How is that useful?  We can swap out different databases, security algorithms, whatever we want -- so long as it returns some kind of `Ref`.  And the contract for `Ref` is fairly small, so it usually doesn't take much to write a little wrapper class to fulfil it.
It is an *ad-hoc* monad, because some time after we have defined it, someone else can still adapt their class into it, and we can use a library that returns their new class without us having to recompile our code.

This has the curious result that we've turned some things monadic that were not before.

`ID` &rarr; `Future[DBItem]` is not a monadic flow

`RefById[DBItem]` &rarr; `RefFuture[DBItem]`  is because both `RefById[T]` and `RefFuture[T]` implement `Ref[T]`. 


## So what do we require `Ref` to fulfil?

I've defined Ref around the sorts of things we need for a typical web app. (Largely because I wrote this for [Impressory](http://impressory.github.io))

* Referring to things by an ID
* Getting the canonical form of an ID
* Things being missing
* Stuff failing
* Knowing the difference between one of something, and many of something
* Getting the result out (which could be asynchronous)
* Ensuring we have a synchronous result if we need to.


### Referring to things by an ID



### Things being asynchronous



### Things being missing, and stuff failing



### One of something and many of something -- plurality

If you have a `Ref[T]`, it refers to precisely one thing.  The plural is `RefMany[T]`.  Going between singles and plurals is remarkably easy.

    for (
      item <- ref;
      thing <- item.getLotsOfThings
    ) yield thing

You now have a `RefMany[thing]`.

    val lots:RefMany[Thing] = getLotsOfThem
    val singularList = lots.toRefOne

You have now gone from a `RefMany[Thing]` to a `Ref[Traversable[Thing]]`

	





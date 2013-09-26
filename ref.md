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

    for {
    	item <- ref;
    	other <- doSomethingWith(item)
    	yetAnother <- doSomethingElseWith(yetAnother)
    } yield yetAnother

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

    val myFoo = LazyId(classOf[Foo], "5242a9ed790e43b81c39b566")

    val idOpt = myRef.getId

#### Canonicalising IDs
    
Just *having* an ID isn't the end of the story. At some point, you'll find you're referring to your ID in a different format. For instance, if you use MongoDB, your IDs are probably ObjectIDs.  But a GET request to your server will have that ID as a String.

And sometimes your ID isn't in a variable called `id`. For example, if you're using MongoDB it may be in `_id`.

For these reason, `Ref.getId` takes an implicit parameter of type `GetsId`.  This knows how to extract an ID from an item, and how to canonicalise an item.  This lets you define ID formats and how they should be converted in one place.

The trait `HasStringId` and its implicit object `GetsStringId` are provided, so if you're using strings as your ID format, just have your data types inherit from `HasStringId`.

#### Setting up how data should be looked up

You'll need to tell handy how to look up your references.  You do this by setting `RefById.lookupMethod` and `RefManyById.lookupMethod`.  The code for these is going to depend on how you do your data storage.  But note that you can freely keep different classes in wildly different kinds of data storage (eg, users are in this SQL database over here, while documents are in that asynchronous MongoDB store over there) and the `Ref` code will still fit together neatly.

#### Referring to something

I recommend using `LazyId` for looking up items. The reason for this is that `LazyId` keeps both the original ID (it retains a `RefById`) and the item that was resolved (whatever `lookUp` returned).  

Why is that important?  Well, if you're working asynchronously, then your lookup method is going to return a `Future` (wrapped as a `RefFuture`). That means you still don't have the item until some point in the future.  If you need to get the ID back out again, for another step in your algorithm, it's handy not to have to wait for the future to complete.  And if you ask for the item from the `LazyId` the second time, it will give you the `RefFuture` it obtained the first time rather than trying to look it up again.  So, you can just pass the `LazyId` around knowing that both requests for its ID and requests for the item itself are going to work well.

### Things being asynchronous

    val futureFoo:Future[Foo] = takesALongTime

    val refFoo = futureFoo.toRef

If you're doing asynchronous work, then you'll probably find that at the end of your algorithm, you have a `RefFuture` or a
`RefFutureRef`.  But they're still `Ref`s.


### Things being missing

    val someFoo = Some("foo")

    val refFoo = someFoo.toRef

### stuff failing

    val tryFoo = Try { thisMightThrowAnException }

    val refFoo = tryFoo.toRef


### One of something and many of something -- plurality

If you have a `Ref[T]`, it refers to precisely one thing.  The plural is `RefMany[T]`.  Going between singles and plurals is remarkably easy.

    for {
      item <- ref
      thing <- item.getLotsOfThings
    } yield thing

You now have a `RefMany[thing]`.

    val lots:RefMany[Thing] = getLotsOfThem
    val singularList = lots.toRefOne

You have now gone from a `RefMany[Thing]` to a `Ref[Traversable[Thing]]`

	





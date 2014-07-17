## Handy

**Handy** is a small library that makes writing asynchronous applications easier. It has a simple but expressive way of writing security rules. It has a simple way of handling lazy lookups. It keeps everything neatly separable, so you're not too tighly coupled to your database or web framework.

### How to obtain it

Add this to your build.sbt file

    resolvers += Resolver.sonatypeRepo("snapshots")

    libraryDependencies += "com.wbillingsley" %% "handy" % "0.6.0-SNAPSHOT"

There are also some (entirely optional and incredibly small) modules for working with Play! 2.0 web framework, or working with MongoDB, though they are less mature: 

    libraryDependencies += "com.wbillingsley" %% "handy-play" % "0.6.0-SNAPSHOT"

    libraryDependencies += "com.wbillingsley" %% "handy-reactivemongo" % "0.6.0-SNAPSHOT"


## Why handy?

I don't like my applications to be too closely tied to any particular web framework or database driver. I worry that if they are, I'll spend too much time having to chase their updates. I also like to make it easy for me to use different databases together in the same application. 

So I like to make sure that the places where I'm converting from my application API types to their web framework or database types (and calling their APIs) is small and well-defined. 

And I like my permission rules to be expressed in my code, not in the web framework. Because that way I can do things like decide to drop the web framework and make a native app without having to rewrite all my permission rules. And sometimes those permission rules are things like "students can only edit their work before the assignment deadline" that are hard to express in most web application security frameworks, but dead easy with handy.

I also like my application to be asynchronous and make that easy to. 

So, what's in handy...

### Id

This is a typed ID class. That means it's impossible to accidentally assign an ID of one thing (eg, a user ID) to a lookup that expects something else (eg, a page).  

These are all equivalent:

    "1234".asId[User]
    Id("1234").of[User]
    new Id[User, String]("1234")
    
If we want to keep a sequence of Ids, we use `Ids`

    Seq(1,2,3).asIds[User]
    Ids(Seq(1,2,3).of[User]
    new Ids[User, Int](Seq(1, 2, 3))
    
    
### Lookups

A `LookUp` bundles two function together.

* For `Id`, a function `Id[T,K] => Ref[T,K]`
* For `Ids`, a function `Ids[T,K] => RefMany[T]`

I'll explain `Ref` and `RefMany` in a moment (they are very simple), but for the moment just read `Ref[T]` as "that's probably a `Future[T]`", and `RefMany[T]` as "that's probably a `Future[TraversableOnce[T]]`

Because our lookups are just a pair of functions, we can happily have different lookups for different types of item. And to use a different database, we just use a lookup containing a different function.

### LazyId

A `LazyId` has an `Id` and a lookup function. If you call lookup on it, it will store the result in a lazy val so that next time you call lookup it won't need to call the function again.

But if you don't call lookup, and just ask for the ID, it'll just give you the ID.

The easy way to get a `LazyId` is

    "123".asId[User].lazily(lookup)
    
or if your lookup is implicitly in scope

    "123".asId[User].lazily


### Ref

`Ref` is a little type that lets us treat `Future[T]`, `LazyId[T, K]`, `Option[T]`, `Try[T]`, and a few others as "a reference to something".  `Ref` is a trait, and there are little wrappers (adapters) for Future, Try, and Option. LazyId itself meets the trait.

It's particularly useful because it lets us declare functions that take a `Ref`, and we can happily pass either a `Future` or a `LazyId` to them. And the body of the function can then decide whether it needs to get the value or just its ID. You'll see an example of this being surprisingly useful when we talk about permissions.


### Approval

`Approval` is a little wallet that remembers what a user has been approved to do in a call. You call it like this

    approval ask permission
 
It returns a `Ref[Approved]` (which can be a wrapper around `Future[Approved]`, or `Try[Approved]` etc).

`Approved(message)` just indicates something has been approved.
`Refused(message)` is an exception indicating something has not been approved -- and works well with `Future`'s and `Try`'s ways of indicating failure. 

### Permissions

Permissions in handy are objects. So that they can be cached. There are two simple kinds:

* Unique permissions
* Permissions on an Id


Permissions on an Id take a parameter. But they've been engineered so that the parameter can be a `Ref`, but the equality check for the permission uses the Id.

So:

    canEditCourse(Future{ course1 }.toRef) == canEditCourse(1.asId[Course].lazily)

Why is that helpful? Well, `Approval` caches permissions that have already been approved. So consider:

    approval ask canEditCourse(1.asId[Course].lazily)
    
If permission to edit course 1 has already been approved, it's in the `Approval`'s cache and the `LazyId` won't even need to be looked up -- we don't need to make a database call. But if it has not already been approved, then it'll need to be resolved, and that probably means looking up the page.

So that's also a neat little example of why having `Ref` helps. Our code stays incredibly simple, but it also helps us avoid unnecessary calls to the database.

Why might a permission be asked for more than once? Well, handy also makes it easy for our permissions to *delegate*.

Here's how a permission might be defined:

    val canEditAssignment = Perm.onId[User, Assignment] {
      case (approval, refAssgt) => for {
        assgt <- refAssgt
        approved ask canEditCourse(assgt.course)
      } yield approved
    }

So to be allowed to edit this assignment, you must be allowed to edit the course. So you can imagine that you could easily have two permissions that require a common permission, and it's useful if that common permission has been cached.

### GetsId

`GetsId` knows how to get an object's id, and knows how to "canonicalise" an id.

Its main use is that if you have a `Future { myobj }.toRef`, we need to know how to get an ID from `myobj` (so that we can do cache checks in the approvals wallet).

It has a second method, `canonicalise`, which also needs to be set. This can be used to do things like convert an Int to String if you use string IDs but sometimes pass ints around. But it's main "need" is internal -- `LazyId[T,K].getId` calls canonicalise because otherwise the type system can't be sure you created the `LazyId` with the same kind of id that `GetsId` returns.

### LookUpCache

A `LookUpCache` is a simple concurrent mutable cache that lets you cache the lookup results of `Id` and `LazyId`. 

    val cached = cache(fooById)



### Another useful thing about Ref

Because we have `Ref`, being this very simple little unifying type, in a typical application, most function just end up being a single `for { ... }` block.

For example:

    def updatePage(approval:Approval[User], pageId:String, data:Json) = {
      rPage = pageId.asId[Page].lazily
      for { 
        approved <- approval ask editPage(rPage)
        updated <- PageModel.update(rPage, data)
        json <- PageToJson.toJson(updated)
      } yield json
    }        

Our main API calls become simple `for` blocks across each piece of behaviour we need to do. And those lower level parts are all neatly separable, so we can swap databases (or even use different databases easily in the same API call).


### Plurality

`RefMany` is the plural equivalent of `Ref`. But they also combine neatly in for blocks.  Suppose a group contains the ids of several users.

    for {
      group <- refGroup
      user <- group.users.lookup
    } yield user.firstName
    
The result is a `RefMany[String]` of every user's first name.

 

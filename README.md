*(This is a bit of a work in progress, and might be edited a lot)*

## Handy

**Handy** is a library I've been writing that has two useful things in it: **Ref** and **Approval**

Essentially, it lets me write my application in a way that is concise, expressive, and straightforward, while also making it easy for me to change database layer or web framework without impacting much on the code in the middle.

Part of the idea is that this library is *small*, which means there's not much complexity going on, and modifying the library to suit your application if it's missing something you need is easy.

The Intelligent Book has shifted database and web-framework a few times in its lifetime, so I've found it useful.

### Ref

**Ref** is about referring to things. 

Sometimes we have objects to pass around. Sometimes we've only got the ID of the object, and haven't gone and looked it up yet.

    def doSomethingWithUser(user: Ref[User]) { ... }

    doSomethingWithUser(RefById(classOf[User], 1234))
    doSomethingWithUser(RefItself(fred))
    
    // or equivalently for the last one
    doSomethingWithUser(fred.itself)

So, we can have a method that accepts a `Ref[User]` and don't have to decide in advance whether that method will actually need to look the user up.

    def doSomethingWithUser(user: Ref[User]) = { ... }

When a **RefById** is looked up, it could turn out that:

1. The record is in the database, or
2. The record isn't in the database, or
3. Something goes pop trying to find out.

And that could be resolved synchronously or asynchronously. 

Different databases might want to return quite different types -- as a simplistic example, perhaps, an `Option[Item]` from one database, but a `Future[Item]` from another. And if our app has more than one database (it's been known to happen) we might even get a different kind of enclosure for different objects.

    val fetchedUser = SimpleBlockingDB.getUser(1234) // Option[User]
    val fetchedPage = SimpleNonBlockingDB.getPage(1234) // Future[Page]

However, it's easy to get a `Ref` from any of these. We have a small but extensible number of `Ref` types that can be returned: `RefFuture`, `RefItself`, `RefNone`, `RefFailed`, etc. That way we can just write our code against `Ref`, and not be coupled too closely to our database types.

`Ref` is also fairly useful when writing `User` and `Page` themselves. For instance, `Ref`ing Salat case classes is rather easy:

    case class Page extends api.Page(
      _id: BSONObjectId = new BSONObjectId(),
      var name:String,
      var content:String,
      val _createdBy:ObjectId
    ) {
    
      // All we need to do to turn createdBy into a Ref:
      def createdBy = RefById(classOf[User], _createdBy)
    
    }

The upshot of this so far is we can quickly write how our program works in terms of `Ref`, and not have to worry too much about if we need to change database or change database driver -- even if we change from blocking to non-blocking and suddenly have `Future`s to deal with, we just return a `RefFuture` that makes it still look like a `Ref`.

So the following code

    for (p <- pageById; u <- p.createdBy; org <- u.organisation) yield org
  
Will still return a `Ref[Organisation]` even if pages are looked up synchonrously and users are looked up asynchronously.


### Plurality

With `RefMany`, we can have the following

    val approval = Approval(loggedInUser)
    val page = RefById(classOf[Page], pageId)
    val results = for (
      a <- approval ask CanRead(page);
      p <- page;
      h <- p.historyItems 
    ) yield { h.toJson }

and the outcome is a `RefMany[JSON]`. And again we can decide later what to have underlying that.

And we have a couple of very human-meaningful types to talk about in APIs.

    def owner: Ref[User]
    
    def editors: RefMany[User]

    def edit(page: Ref[Page], content:String)
    
    def bulkEmail(recipients: RefMany[Contact])    

If we wanted, we could define queries or all sorts of things to return a `RefMany`.


### Approval

So, **Approval** suddenly turned up in our last example. It's designed to play nicely with `Ref`.

An `Approval` is essentially a cache of permissions. Ask it for a permission

    approval ask CanDoFoo
    
and you'll get a `Ref[Approved]` coming back. In the synchronous case, that'll be a `RefItself[Approved]` or a `RefFailed[Refused(message)]`. In the asynchronous case, it might well be a `RefFuture[Approved]`. It's up to you.

But if it's successful, the `Approval` will also cache the permission you asked it for. So in the synchronous case,

    val a1 = Approval ask CanDoFoo
    val a2 = Approval ask CanDoFoo
    
will only ask `CanDoFoo` to resolve whether or not the user can do this once. 

And, `CanDoFoo` gets given a reference to the `Approval` when it's asked. This is so that permissions can resolve to different permissions that might already have been granted:

    case class CanEditPage(page: Ref[Page]) extends Perm[User] {
      def resolve(prior: Approval[User]) = page flatMap { 
        if (_.isProtected) {
          prior ask CanEditProtected
        } else {
          prior ask CanEditUnprotected
        }        
      }    
    }
    



### Unpacking it all

So your business logic is now returning a `Ref[Something]` or a `RefMany[Something]`. Your web framework meanwhile needs to write out HTTP responses. And different web frameworks have different classes for doing this.

So, we do is use an implicit method or two to convert your Refs into whatever your web framework wants.

Now, if you know you're going to be doing everything synchronously, you can just call `ref.fetch` to turn your `Ref` into a `ResolvedRef` and pattern match on it.

    implicit def jsonRefToResponse(r: Ref[JSON]) = {
    
      r.fetch match {
        case RefItself(thing) => Ok(toJson(thing))
        case n:RefNone => NotFound()
        case RefFailed(exc) => exc match {
          case Refused(reason) => Forbidden(reason)
          case _ => InternalServerError(exc)
        }      
      }
    
    }

On the other hand if you are doing anything asynchronously, you'll probably want to turn it into an asynchronous response.

    implicit def jsonRefToResponse(r: Ref[JSON]):Future[Response] = {
      
      Async {      
        val prom = promise[Response]
        r onComplete {
          onSuccess = { json => prom success Ok(json) }
          onNone = { prom success NotFound() }
          onFail = _ match {
            case Refused(msg) => prom success Forbidden(msg)
            case t:Throwable => prom success InternalServerError(t.getMessage)
          }
        }
      }    
          
    }
    
That might look a bit of a pain, but you only have to do it once. 

Most of your controllers look like this:

    val approval = Approval(loggedInUser)
    val page = RefById(classOf[Page], pageId)
    val results = for (
      a <- approval ask CanRead(page);
      p <- page
    ) yield { p toJson }

and implicitly convert into a response.    

We can do similar things with `RefMany`, but for instance in the asynchronous case we might want to take advantage of `Iteratees`.


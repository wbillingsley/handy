---
layout: default
title: Approval
---

# Approval

<p class="lead">
A very easy way of handling security, no matter how complex the rules you have to implement are
</p>

An `Approval` holds all the permissions that have been looked up so far.  Ask it for one that's already
been granted, and it'll come back `RefItself(Approved("Already granted"))`

Ask it for one that hasn't been granted yet, and it'll call `resolve` on the permission

Resolve is defined as 

    def resolve(prior: Approval[T]):Ref[Approved] = {
    	// your implementation
    }

This means that your permission can do things like delegate to other permissions that might already have been granted.

    case class Edit[Page](item: Ref[Page]) extends PermOnIdRef[Item, User](item) {      
      
      def resolve(prior: Approval[T]):Ref[Approved] = {
        for (
        	i <- item;
        	approved <- prior ask (if (i.protected) EditProtected else EditUnprotected)
        ) yield approved    	
      } 
    
    }

You'll notive that `resolve` returns a `Ref[Approved]`.  This means it can be asynchronous, it can handle failures, and it can be very simply integrated in with the rest of your functionality.



## PermOnIdRef

Suppose someone asked for permission to edit a page, passing the page object itself.  Then they ask for permission to edit a course, passing a `LazyId` that points to the same page.  We'd rather not have to look up the page in the database, just to say "Already granted".  We'd like to consider the two permissions equal if they refer to the same item, even if they are different kinds of `Ref`.  This is what `PermOnIdRef` handles.

By default, PermOnIdRef considers two permissions to be the same if they are the same permission, and the `Ref` passed in has the same id.

So, `EditPage(page1.itself)` equals `EditPage(LazyId(classOf[Page], 1))`

### Reusing permissions for multiple classes

By default, `PermOnIdRef` assumes that you use different permissions for different classes.  Say, `EditPage(r:Ref[Page])` for Pages and `EditComment(r:Ref[Comment])` for Comments

If, however, you use the *same* permission class for *different* object classes -- for instance, if you define `GenericEdit(r:Ref[Item])` and define `Page` and `Comment` as subclasses of `Item` -- then you will need to specify one extra parameter.  Because by default two permissions are equal if the permission is the same and the ID on the Ref is the same, even if the `Ref`s point to different classes.

Don't do this:

    prior ask GenericEdit(page1.itself)

because it would be equal to

    prior ask GenericEdit(comment1.itself)

Instead call

    prior ask GenericEdit(page1.itself, classOf[Page])

because that is not equal to 

    prior ask GenericEdit(comment1.itself, classOf[Comment])

The reason this is needed is because the Scala compiler erases the generic type of the Ref at runtime -- at runtime the code doesn't know the difference between a `Ref[Page]` and a `Ref[Comment]`

In future, we'll use TypeTags to preserve this information at runtime, so you won't need to pass the class parameter, but I'm waiting on the powers that be to fix a thread-safety bug in Scala's use of TypeTags first.


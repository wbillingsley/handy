---
layout: default
title: Handy
---

# Making complex apps a little bit easier

<p class="lead">
Handy is a set of classes that make writing apps in a functional style really quite easy.
</p>

How easy?  Here's an example.  (We'll make it even easier in a mo.)

    val approval = request.approval
    for (
      text <- Ref((request.body \ "text").asOpt[String]) orIfNone UserError("The message contained no text");
      entry <- refContentEntry(entryId);
      approved <- approval ask Permissions.CommentOnEntry(entry.itself);
      updated <- ContentEntry.addComment(entry, approval.who, text);
      j <- updated.toJsonFor(approval)
    ) yield j

In those few lines, we've:

* Retrieved the input comment, including error-handling if there wasn't one
* Looked up the item in the database, including both error-handling and handling the case where the entry doesn't exist
* Handled security on whether the user should be allowed o comment
* Updated the database
* Converted the response to JSON
* If all was successful, sent it back to the client, and if unsuccessful sent an appropriate 404 or 500 response.

And we've done it in an asynchronous non-blocking manner.

## handy-appbase-core

If you're using the [Play Framework](http://playframework.com), then you can also use some code that's in `handy-play` and `handy-appbase-core`.  

* `handy-play` adds some code for producing Enumerators that work with Play's iteratee library
* `handy-appbase-code` adds some code for Single Page Applications

This is the example above filled out to use `handy-appbase-core` to define a Play controller action

    def commentOnEntry = DataAction.one(parse.json) { implicit request =>
      for (
        text <- Ref((request.body \ "text").asOpt[String]) orIfNone UserError("The message contained no text");
        entry <- refContentEntry(entryId);
        approved <- request.approval ask Permissions.CommentOnEntry(entry.itself);
        updated <- ContentEntry.addComment(entry, approval.who, text)
      ) yield updated
    }

Here, we've defined a controller action that has some helpful propertes:

* If the client says it accepts HTML, it will return the base HTML for your single page app.  (A browser has come to your app using a path that corresponds to a REST request)
* If the client says it accepts JSON, it will convert the `Content` to JSON using a JSON converter you have defined. That can involve more aysnchronous calls if you wish.  (For instance if you want to fetch and embed other items into the response too.)
* If the call fails, an appropriate HTTP error code will be set. `Forbidden` if the permission is refused, `InternalServerError` if there's an unexpected error, and usually `UserError` is configured to return `BadRequest`.

## handy-reactivemongo

Many of my projects use ReactiveMongo as the database driver, so there's also a couple of classes provided for that.




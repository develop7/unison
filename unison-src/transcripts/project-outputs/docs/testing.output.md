### Testing and watch expression caching

First, let's get this out of the way. One uncontroversial, status quo way to do testing in Unison: just use a regular main function. So, by convention, your branch might have a term, `tests : '{IO} Boolean` or perhaps `[Text] ->{IO} Boolean`, if it has an EasyTest-like interface where you can select scopes dynamically. You then do:

``` 
> execute tests
```

In your branch and it runs your tests and prints out some nice emojis. If you're running `tests` standalone and care about exit codes, you probably take the `Boolean` you get, where `true` indicates success and `false` indicates a failure, and convert that to an exit code. Assuming `IO` has some way of exiting with an exit code.

(Note, we don't have a way of launching `unison` to run some commands on a branch then quit yet, but we probably will have something like `unison mybranch execute tests ["math"]`.)

Easy peasy. Nothing special we need to do to enable this mode of working, and it's going to be how you do any sort of integration tests that need to talk to the outside world.

### Easy incremental testing for pure tests (the "tests cache")

But, when tests *aren't* in `IO`, there's no need to rerun them unless one of their dependencies changes (though you can if you want to). A simple proposal which lets us take advantage of this is we allow watch expressions to be marked as tests. They have to be of type `Test.Status`:

``` 
type Test.Status = Failed Text | Passed Text
```

> We debated whether to make tests have more structure and came down on "no" - different testing combinator libraries or abilities can handle all that, and this `Test.Status` is more like a final compilation target for different testing APIs: the test passed or failed, and has some human-readable information in it. That's it.

And to mark a watch expression as a test, you say:

``` Haskell
test> Test.equal (sort [3,1,2]) [1,2,3]
```

> Hmm, what if your test needs a whole bunch of auxiliary definitions and doesn't fit in a single watch expression? Easy, just introduce regular definitions for these, perhaps with some simple naming convention (like I'd prefix these testing helper definition names with `tests.`).

> Did you consider just keying off the type of the watch, like if it's of type `Test.Status`, assume it's a test? Yes we did, but we decided being explicit was better. Also by communicating your intent up front, you can get better feedback from the tool ("er, looks like this isn't a test, here's how you can make it one") vs silently ignoring the thing the user thought was a test and just not adding it to the branch.

On `add`, these `test>` watches are added to the codebase. Watch expressions marked as `test>` are also added to the namespace of the branch and given some autogenerated unique name (perhaps just computed from the hash of the test itself), unless the watch expression picks a name as in `test> test.sortEx1 = ...`. The user is told these names on `add`/`update` and can always rename them later if they like. Don't forget that in the event of a test failure, Unison can also show you the full source of the failed watch expression. Also note that the `Passed` and `Failed` cases might include the name of the "scope" of the test or other relevant info. So I'm not sure how important these names will be in practice

There's a directory, `tests/`, containing files of the form `<hashXYZ>.ub`. The `hashXYZ` is a reference to the source of the original watch expression (in this case, the `Test.equal (sort [3,1,2]) [1,2,3]`), and the `.ub` file itself is a serialized `Test.Status`. We can ask if a branch is passing just by taking the intersection of the hashes in the branch with the hashes in this directory and seeing if all the `Test.Status` values for the branch are `Passed`. Notice this doesn't involve running any of the tests\!

Since these test watches are part of the branch, they get refactored just like everything else when their dependencies change. Nothing special there, which is nice. We suggest that `update` rerun any changed tests by default. Here's how that works:

  - On `update`, we check the `tests/` directory and compare the hashes there to the edits list in the branch. If there's a file `<hashXYZ>.ub`, and the branch has an edit `hashXYZ -> hashPQR`, we lookup the source of `hashPQR` and evaluate it, and store the result in `<hashPQR>.ub`. We do this for any affected tests.

The `tests/` directory will be versioned, so everyone collaborating on the code shares a cache of test results. As the tests are 100% deterministic, this is fine, unless of course someone manually mucks with that directory to doctor some test results, or if like a freak gamma ray corrupts your test as it's running and gives the wrong result. But note that you can always choose to rerun some or all of your tests, ignoring the cache - just lookup the source of the `<hashXYZ>` and recompute it. (And perhaps there's a command to do that in bulk for a whole branch.) If it doesn't match, you can then hunt down the person who added that bogus test result. :)

### Caching watch expressions (the "watches cache")

Same idea, except that the source of a watch expression isn't added to the codebase. We just have a `watches/` directory in the same spot, with files `<hashXYZ>.ub` in it, which contain the evaluated result of the watch whose source was `hashXYZ`. Optionally, `watches/` directory could be in some other user-configurable location.

When evaluating a Unison file, we have to hash all its definitions. If one of those hashes matches a hash in the `watches/` directory, we skip its evaluation and return the cached value.

This caching can be done by default, but I suggest that the `watches` directory *not* be versioned as the values might be quite large. However, I could see people wanting to share their watches cache and sticking it on some shared file system.

### Implementation notes and remarks

We will neeed the list of watches in `UnisonFile` to include extra information: what kind of watch expression is it? A test or a regular watch? We'll then need to make use of this information on `add` and `update`. And we might want to expose other commands for rerunning tests anyway.

Aside: I kinda like the "trust but occasionally reverify" model for this kind of caching. So every once in a while, pick a random test to rerun and make sure it checks out. With statistics, over time, it becomes exceedingly likely that the cache is good and any somehow incorrect results will be caught. Pessimistically rerunning all the tests, all the time, is Right Out. :)
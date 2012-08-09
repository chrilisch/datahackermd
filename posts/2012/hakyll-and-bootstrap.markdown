---
title: Hakyll and Bootstrap
author: Akshay Shah
published: 2012-08-04T19:00:00Z
preview: Monadic <abbr class="initialism">I/O</abbr> hurts so good
---

Setting up this site has been a fantastic opportunity to indulge myself and
tinker with some new tools &mdash; I've run a few [WordPress][] sites in the
past, but I've never taken the time to explore much beyond that. After spending
a few weeks playing with the newest version of WordPress, some [full-fledged
<abbr class="initialism">CMS</abbr>s][Drupal], and hosted services like
[Tumblr][] and [Posterous][], I've decided to use [Hakyll][] and [Bootstrap][].

## Hakyll: Static Site Generation

Hakyll is a static site generator: it takes a series of blog posts, each
written in [Markdown][], and combines them with a few templates to produce a
complete website. Once that compilation step is complete, each page of the site
is a single <abbr class="initialism">HTML</abbr> file which can be uploaded to
a server and sent to readers as-is, without any on-the-fly substitutions or
database queries. Static sites have some big advantages:

* They're fast by default. Since serving pages doesn't involve any fancy
  processing that might slow things down, I can focus on writing instead of
  optimizing site speed.
* For many of the same reasons, static sites use very little memory. On the
  inexpensive shared hosting plan I use, every little bit of memory matters.
* Site backups are painless. Rather than trying to periodically backup a
  database, I keep the templating backbone and the content of the site in a
  [git][] repository that's cloned onto several computers. I can always revert
  to an earlier version of the site, and I won't lose any data even if half the
  Internet goes up in flames. In contrast, most <abbr
  class="initialism">CMS</abbr>s are difficult to version and backup because
  all the data lives in a database.
* It's easy to write new content. Writing a post in most web-based tools
  requires logging in and struggling with a half-baked, online version of
  Microsoft Word.  With a static site, I can work offline &mdash; in whichever
  editor I choose &mdash; and publish my work later with a few quick commands.

That said, there's no practical way for me to run my own comment system on a
static site. [Disqus][] and [IntenseDebate][] offer easy JavaScript-based
workarounds, but I'll need to do a little more research before I'm ready to
trust them with much data.

I'm certainly not the only one to have realized that static sites solve a lot
of problems &mdash; they're all the rage among geeks these days, and it seems
like someone announces a new static site generator every week. Using Hakyll was
an easy choice, though, because it's written in [Haskell][]. I've been itching
to learn some Haskell for a while, and this seemed like as good a time as any to
dive in. Configuring a new Hakyll site is fairly straightforward, even with my
nonexistent Haskell skills, and the [documentation][hakyll-docs] is excellent.
I'm still struggling to understand monads, arrows, and fields, but [Hakyll's
author][jasper] is helpful and active on the [mailing list][hakyll-list].

## Bootstrap: Clean Design, No Fuss

I have trouble matching my clothing, let alone the dozens of small design
elements that make up a website, so actually writing templates for my new site
was a daunting task. Luckily, nobody needs to see my first efforts &mdash; I
decided to use Twitter's Bootstrap framework instead. It's clean, attractive,
phone- and tablet-friendly, and it's teaching me some of <abbr
class="initialism">HTML</abbr>5's new tricks. Best of all, it doesn't slow the
site down too much because most of the Javascript components are optional.

## Future Improvements

Though Hakyll's commands are faily concise, it's getting tiresome to type each
one out repeatedly. I'd love to automate my build-preview-publish cycle more,
but I'm trying to get away from using shell scripts for everything. This is a
golden opportunity to learn a little about [Fabric][], but it does feel like
cheating to jump straight to Fabric without appreciating the pain of <abbr
class="initialism">GNU</abbr> make.

Though I'm not quite sure what to change, I'd also like to customize Bootstrap
a bit. Using [{less}][less] as a styling meta-language has been an easy
transition, but my attempts to alter the default colors and fonts have been
truly hideous. Until I find my muse, I'll just have to live with a site that
looks very similar to thousands of others.

<div class="pagination-centered">
  <a class="btn btn-primary" style="margin: 1em 0;" href="http://github.com/akshayjshah/datahackermd" title="Source Code on GitHub"><i class="icon-github"></i> Browse the Source</a>
</div><!-- .pagination-centered -->

[WordPress]: http://www.wordpress.com "WordPress"
[Drupal]: http://drupal.org/ "Drupal"
[Tumblr]: https://www.tumblr.com/ "Tumblr"
[Posterous]: https://posterous.com/ "Posterous Spaces"
[Hakyll]: http://jaspervdj.be/hakyll/ "Hakyll"
[Bootstrap]: http://twitter.github.com/bootstrap/ "Twitter Bootstrap"
[Django]: https://www.djangoproject.com/ "Django"
[Markdown]: http://daringfireball.net/projects/markdown/ "Markdown"
[git]: http://git-scm.com/ "Git"
[Disqus]: http://disqus.com "Disqus"
[IntenseDebate]: http://intensedebate.com "IntenseDebate"
[Haskell]: http://www.haskell.org/haskellwiki/Haskell "HaskellWiki"
[hakyll-docs]: http://jaspervdj.be/hakyll/tutorials.html "Hakyll Tutorials"
[jasper]: http://jaspervdj.be "Jasper Van der Jeugt"
[hakyll-list]: https://groups.google.com/forum/?fromgroups#!forum/hakyll "Hakyll Google Group"
[less]: http://lesscss.org "{less}"
[Fabric]: http://docs.fabfile.org/en/1.4.3/tutorial.html "Fabric"

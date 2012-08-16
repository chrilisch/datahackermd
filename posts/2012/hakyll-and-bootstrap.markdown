---
title: Hakyll and Bootstrap
published: 2012-08-04T19:00:00Z
updated: 2012-08-12T22:00:00Z
preview: Monadic <abbr class="initialism">I/O</abbr> hurts so good
---

Though I've run a few [Wordpress][] sites in the past, I've always found the
software irritating to use. In particular, I *loathe* the composer &mdash; the
rich text editor never works quite as I expect it to, and the <abbr
class="initialism">HTML</abbr> editor mangles my markup without warning. The
installation is dead simple, the theme support is fantastic, and I love [Matt
Mullenweg's support of free software][gpl], but Wordpress just isn't for me.
At the same time, I don't miss hand-coding entire sites; I've done that exactly
twice, and maintaining scads of duplicated markup is a nightmare I don't care
to repeat.  Since I'm starting this site with a clean slate, I have the luxury
of choosing whatever tools I like; after looking at everything from [Drupal][]
to [Tumblr][], I decided to keep things simple and build with [Hakyll][] and
[Bootstrap][].

## Hakyll: Static Site Generation

Hakyll is a static site generator, which means that it's only a few short steps
away from writing markup by hand. Those few steps make all the difference,
though &mdash; I can still control exactly what the final <abbr
class="initialism">HTML</abbr> will look like, but Hakyll lets me write
articles in [Markdown][] and use templates for site-wide elements like
navigation and footers.  Static sites are all the rage among geeks
these days, so [a lot's][nanoc] already [been written][stevelosh] about their
technical advantages.  In short, static sites are:

* fast by default, even on inexpensive hardware;
* immune to many common security exploits;
* easily saved and versioned; and
* editable offline.

Most importantly for me, Hakyll is comfortable. I can write templates and posts
in [vim][], keep versions and branches in [git][], and generally work with
whatever text manipulation tools make me happy. Hakyll even comes with a
built-in webserver, so it's easy to see a live preview of any changes I'm
making. Since writing on this site is supposed to be a fun side project,
comfortable tools are priority zero.

Building a site without a database isn't all roses and puppies, though. There's
no easy way for me to include an automated "Popular Posts" widget in the
sidebar, for example, and any future search widgets will need to rely on an
external search engine. Most importantly, it's impossible for me to store and
manage reader comments. Services like [Disqus][] and [IntenseDebate][] offer
easy Javascript-based workarounds, but I'll need to do a little more due
diligence before I'm comfortable trusting them with critical data.
(*Update*: I've added Disqus comments to all posts. However, the widget renders
so poorly on small screens that I've hidden it on devices smartphone-sized and
smaller.)

Hakyll also has its own set of challenges, mostly because it's written and
configured in [Haskell][]. To put it mildly, I'm a Haskell neophyte &mdash;
I've been interested in the language for about a year, but haven't done
anything more than a few [Project Euler][] questions. Since I also don't have a
strong background in category theory, monads and arrows are *blowing my mind*.
There's something really amazing and elegant going on, but I'm only catching
glimpses of it between compiler errors. Nevertheless, the Hakyll
[documentation][hakyll-docs] is excellent, the [mailing list][hakyll-list] is
active, and the [author][jasper] is exceptionally helpful, so my first foray
into practical functional programming has been more enlightening than
infuriating.

## Bootstrap: Clean Design, No Fuss

I have trouble matching my clothing, let alone the dozens of small elements
that make up a website, so creating an attractive design for my new site was a
daunting task. Luckily, nobody needs to see my first efforts &mdash; I decided
to use Twitter's [Bootstrap][] framework instead. It's clean, attractive, and
mobile-friendly, and it's teaching me some of <abbr
class="initialism">HTML</abbr>5's new tricks. [{less}][less], the <abbr
class="initialism">CSS</abbr> meta-language Boostrap uses, is also wonderful:
 it's close enough to vanilla <abbr class="initialism">CSS</abbr> that
it's easy to learn, but it's made my stylesheets much more modular and
consistent.

While I haven't tweaked Bootstrap's default styling much, I *had* to do
something about the fonts. I like Helvetica, especially on visually intense
marketing sites &mdash; but Bootstrap's tiny default font size combined with
Helvetica's clinical modernism made blocks text downright hostile. After a few
hours poking through [Google Web Fonts][webfonts] and testing different styles,
I settled on [Omnibus Type's][omnibus] [Rosario][rosario]. To my eye, it
manages to be a little more playful and human than Helvetica without
distracting from the words themselves.

My efforts to choose a different color scheme, though, have been a complete
failure. The defaults are nice enough, but they lack soul, and my efforts to
change them usually end in a neon-tinted nightmare. There's hope on the
horizon, though &mdash; I just read Ian Taylor's ["Never Use Black,"][black]
and I may try mixing some blue or red into the default grays.

I'm not much of a programmer or a designer, so I'm always in the market for
suggestions! If coding's your thing, take a look at the source code on GitHub;
otherwise, send me an [email](mailto:akshay@datahackermd.com) or a
[tweet](http://twitter.com/akshayshah) and let me know what you think.

<div class="pagination-centered">
  <a class="btn btn-primary" style="margin: 1em 0;" href="http://github.com/akshayjshah/datahackermd" title="Source Code on GitHub"><i class="icon-github"></i> Browse the Source</a>
</div><!-- .pagination-centered -->

[gpl]: http://ma.tt/tag/gpl/ "Matt Mullenweg on the GPL"
[WordPress]: http://www.wordpress.com "WordPress"
[Drupal]: http://drupal.org/ "Drupal"
[Tumblr]: https://www.tumblr.com/ "Tumblr"
[Hakyll]: http://jaspervdj.be/hakyll/ "Hakyll"
[Bootstrap]: http://twitter.github.com/bootstrap/ "Twitter Bootstrap"
[nanoc]: http://nanoc.stoneship.org/docs/1-introduction/ "Nanoc Documentation"
[stevelosh]: http://stevelosh.com/blog/2010/01/moving-from-django-to-hyde/ "Steve Losh: Moving from Django to Hyde"
[Markdown]: http://daringfireball.net/projects/markdown/ "Markdown"
[git]: http://git-scm.com/ "Git"
[Disqus]: http://disqus.com "Disqus"
[IntenseDebate]: http://intensedebate.com "IntenseDebate"
[Haskell]: http://www.haskell.org/haskellwiki/Haskell "HaskellWiki"
[hakyll-docs]: http://jaspervdj.be/hakyll/tutorials.html "Hakyll Tutorials"
[jasper]: http://jaspervdj.be "Jasper Van der Jeugt"
[hakyll-list]: http://groups.google.com/group/hakyll "Hakyll Google Group"
[less]: http://lesscss.org "{less}"
[webfonts]: http://www.google.com/webfonts "Google Web Fonts"
[omnibus]: http://www.omnibus-type.com/ "Omnibus Type"
[rosario]: http://www.google.com/webfonts/specimen/Rosario "Rosario"
[black]: http://ianstormtaylor.com/design-tip-never-use-black/ "Design Tip: Never User Black"
[vim]: http://stevelosh.com/blog/2010/09/coming-home-to-vim/ "Steve Losh: Coming Home to Vim"
[Project Euler]: http://projecteuler.net/ "Project Euler"

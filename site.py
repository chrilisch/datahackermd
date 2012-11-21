import logging
import subprocess

from flask import Flask, Response, render_template
from flask_flatpages import FlatPages
from flask_frozen import Freezer
from markdown import markdown
from sys import argv
from typogrify.filters import typogrify


logging.basicConfig(level=logging.DEBUG)


MARKDOWN_EXTRAS = [
    'abbr',
    'smart_strong',
    'fenced_code',
    'codehilite',
    'sane_lists']


def typeset(md):
    html = markdown(md, extensions=MARKDOWN_EXTRAS)
    typogrified_html = typogrify(html)
    return typogrified_html


DEBUG = True
FLATPAGES_ROOT = ''
FLATPAGES_AUTO_RELOAD = DEBUG
FLATPAGES_EXTENSION = '.markdown'
FLATPAGES_HTML_RENDERER = typeset
FREEZER_DESTINATION = '../datahackermd-build'
FREEZER_REMOVE_EXTRA_FILES = False  # Keep .git!


app = Flask(__name__)
app.config.from_object(__name__)
freezer = Freezer(app)
posts = FlatPages(app)


@app.route('/')
def post_list():
    return render_template('posts.html', posts=posts)


@app.route('/<path:path>/')
def post(path):
    post = posts.get_or_404(path)
    return render_template('post.html', post=post)


@app.route('/404.html')
def error404():
    return render_template('404.html')


@app.route('/static/style.min.css')
def style():
    less = subprocess.check_output([
        'lessc',
        '--yui-compress',
        'less/style.less'])
    return Response(less, mimetype='text/css')


@app.route('/sitemap.xml')
def sitemap():
    sitemap = render_template('sitemap.xml', posts=posts)
    return Response(sitemap, mimetype='application/xml')


@app.route('/CNAME')
def cname():
    # CNAME record for GitHub Pages
    return 'datahackermd.com', 200, {'Content-Type': 'application/octet-stream; charset=utf-8'}


@freezer.register_generator
def page_generator():
    for post in posts:
        yield 'post', {'path': post.path}


if __name__ == '__main__':
    if len(argv) > 1 and argv[1] == 'build':
        freezer.freeze()
    else:
        app.run(port=8000)

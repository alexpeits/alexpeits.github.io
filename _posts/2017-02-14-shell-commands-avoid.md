---
layout: post
title: "Avoid slow commands in shell startup"
categories: shell
---

If you are constantly starting up terminals to execute one-off commands, some
slow directives in the startup file (bashrc, zshrc or similar) can be very
frustrating.

For example, I use [nvm][nvm] for work right now, in order to write a react
app in node v6, by using code that was written in an earlier version.

What **nvm** does to install itself is simply clone the repository into your
home directory, and append the following code into your shell startup (I use
zsh):

{% highlight shell %}
export NVM_DIR="/home/alex/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
{% endhighlight %}

Turns out the second line takes ~1s to complete, which, to me, is a bit annoying.
I also don't use node or npm that much, apart from this project, so I did the
following at first:

{% highlight shell %}
nvminit() {
    unset -f npm
    export NVM_DIR="/home/alex/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
}
{% endhighlight %}

So, whenever I needed to use `npm`, I would do `nvminit` then proceed. But what
about opening multiple shells to run several apps simultaneously? For example,
a node server, a react app with webpack or what have you. Having to remember to
do `nvminit` **every single time** is equally frustrating. So, why not have the shell
figure out if `npm` is loaded, and call `nvminit` if it is not?

{% highlight shell %}
nvminit() {
    unset -f npm
    export NVM_DIR="/home/alex/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
}

npm() {
    if [ -x npm ]; then
        command npm $*
    else
        nvminit && command npm $*
    fi
}
{% endhighlight %}

Let's break it up. The `npm()` function serves as a wrapper around the `npm` command.
So, you can type, let's say, `npm start`, and it checks if `npm` is an executable. If
it is, then it means that you can run the regular one, passing any arguments to it.
The `command` command is there to avoid recursion, because calling `npm $*` would call
the `npm` **function**, and so on. If it's not defined, then run `nvminit`, and then
anything else.

Turns out that having a custom `npm` function while trying to source the nvm files also
leads to infinite recursion and breaks, so the simple solution is to unset the npm
**function** by using `unset -f`. So, after calling `nvminit`, your shell sees the npm
command as the regular one:

{% highlight none %}
$ which npm
npm () {
	if [ -x npm ]
	then
		command npm $*
	else
		nvminit && command npm $*
	fi
}
$ npm list
/home/alex
└── (empty)

$ which npm
/home/alex/.nvm/versions/node/v6.9.5/bin/npm
{% endhighlight %}

[nvm]: https://github.com/creationix/nvm

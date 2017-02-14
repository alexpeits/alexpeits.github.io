---
layout: post
title: "Formatting timesheet tables in org-mode"
categories: emacs org-mode
---

I recently found the need to become more efficient in the way I would clock my work time, for personal purposes,
as well as for having a presentable and detailed per-day timesheet, for logistics (remote work and such).

Of course, org-mode to the rescue.

As many users of org-mode already know, you can clock a task by using the internal org-mode clock. Just navigate to
a header and `C-c C-x C-i` to start clocking (clock-in), `C-c C-x C-o` to stop (clock-out). I won't go into many
detail here, the docs cover the topic more thoroughly.

**Quick note:** I use org-mode version 9, which by default puts clock entries in a `LOGBOOK` drawer. I have disabled
this by doing `(setq org-clock-into-drawer nil)` in my `.emacs`. I haven't tried to fix this method to work with
drawers (yet).

Let's say you have the following structure:

{% highlight none %}
* Many tasks

** First task
   CLOCK: [2017-02-04 Sat 14:40]--[2017-02-04 Sat 15:50] =>  1:10
   CLOCK: [2017-02-02 Thu 23:30]--[2017-02-03 Fri 02:00] =>  2:30

** Second task
   CLOCK: [2017-02-05 Sun 16:00]--[2017-02-05 Sun 17:00] =>  1:00
   
** Third task
   CLOCK: [2017-02-04 Sat 13:00]--[2017-02-04 Sat 14:30] =>  1:30
{% endhighlight %}

The first obvious solution is to create a **clock report**. Do this with `C-c C-x C-r` while on a header that includes
org-clock entries. This produces the following:

{% highlight none %}
#+BEGIN: clocktable :maxlevel 2 :scope subtree

#+END:
{% endhighlight %}

Not so useful. Thankfully, org-mode clock reports support some directives. By changing the `#+BEGIN` line to 
`#+BEGIN: clocktable :maxlevel 2 :scope file` and then doing `C-c C-x C-r` on it, it renders the following:

{% highlight none %}
#+BEGIN: clocktable :maxlevel 2 :scope file
#+CAPTION: Clock summary at [2017-02-12 Sun 15:46]
| Headline        | Time   |      |
|-----------------+--------+------|
| *Total time*    | *6:10* |      |
|-----------------+--------+------|
| Many tasks      | 6:10   |      |
| \_  First task  |        | 3:40 |
| \_  Second task |        | 1:00 |
| \_  Third task  |        | 1:30 |
#+END:
{% endhighlight %}

A lot better. But again, what we want is a **per-day** report. By again changing the first line to
`#+BEGIN: clocktable :maxlevel 2 :scope file :block thismonth :step day`, we get the following:


{% highlight none %}
#+BEGIN: clocktable :maxlevel 2 :scope file :block thismonth :step day

Daily report: [2017-02-01 Wed]
| Headline     | Time   |   |
|--------------+--------+---|
| *Total time* | *0:00* |   |

Daily report: [2017-02-02 Thu]
| Headline       | Time   |      |
|----------------+--------+------|
| *Total time*   | *0:30* |      |
|----------------+--------+------|
| Many tasks     | 0:30   |      |
| \_  First task |        | 0:30 |

Daily report: [2017-02-03 Fri]
| Headline       | Time   |      |
|----------------+--------+------|
| *Total time*   | *2:00* |      |
|----------------+--------+------|
| Many tasks     | 2:00   |      |
| \_  First task |        | 2:00 |

...and more, one for each day of the month

#+END:
{% endhighlight %}

So far so good. This is a perfectly acceptable format, although it is only useful in
plain text format, or in html/pdf export. Having a csv file or even better an excel file
would be really handy. Unfortunately, org-mode does not offer a simple solution that I
know of, that can put everything in **one single table**, while retaining the per-day
format.

Enter the **Org Element API**.

The Org Element API is essentialy a baked in way to parse a buffer or a subtree or anything
you wish, in a programmatic way, and extract arbitrary data. Or even, I don't know, BUILD
TABLES?

Being on the process of learning emacs lisp, parsing and making it output to a table was not
a really easy task. Thankfully, an answer in stack overflow solved all my problems. I was so
excited, I even forgot to bookmark it and can't find it right now (will update). It goes like
this:

{% highlight none %}
#+NAME: timesheet_table
#+BEGIN_SRC elisp :results table
  (nconc
   '(("month" "day" "clockin" "clockout" "duration" "title"))
   '(hline)
   (let ((ast (org-element-parse-buffer 'element)))
     (org-element-map ast 'clock
       (lambda (x)
         (let ((val (org-element-property :value x)))
       `(,(calendar-month-name (org-element-property :month-start val) t)
         ,(number-to-string (org-element-property :day-start val))
         ,(concat (number-to-string (org-element-property :hour-start val))
              ":"
              (format "%02d" (org-element-property :minute-start val)))
         ,(concat (number-to-string (org-element-property :hour-end val))
              ":"
              (format "%02d" (org-element-property :minute-end val)))
         ,(org-element-property :duration x)
         ,@(org-element-map
           (org-element-property :parent (org-element-property :parent x))
           'headline
             (lambda (x) (org-element-property :title x))))))))
  )
  )
#+END_SRC
{% endhighlight %}

This block does the following:

It uses `nconc` to **concat** multiple expressions into a list. At first it inserts
the headings and a horizontal line `hline`. Then, it parses the current buffer using the
Element API, and it maps over all the **clock** elements.

These elements provide some useful attributes, such as `:year-start`, `:month-start` and so
on. It's pretty straightforward from then on. The commas in front of some sexps are used in
conjuction with the backtick, and instruct lisp that it should evaluate the sexp, rather than
returning it verbatim. The `@` sign helps "unpack" of "flatten" the elements of the following
list into the outer one (or something like this). Check out the [docs][docs-backquote] for
some more detail.

The previous code block produces the following table:

{% highlight none %}
#+RESULTS: timesheet_table
| month | day | clockin | clockout | duration | title       |
|-------+-----+---------+----------+----------+-------------|
| Feb   |   4 |   14:40 |    15:50 |     1:10 | First task  |
| Feb   |   2 |   23:30 |     2:00 |     2:30 | First task  |
| Feb   |   5 |   16:00 |    17:00 |     1:00 | Second task |
| Feb   |   4 |   13:00 |    14:30 |     1:30 | Third task  |
{% endhighlight %}

Which is awesome! You can even do more stuff, like adding a calc formula for outputting the
sum of hours:


{% highlight none %}
#+NAME: timesheet_table
#+BEGIN_SRC elisp :results table
  (nconc
   '(("month" "day" "clockin" "clockout" "duration" "title"))
   '(hline)
   (let ((ast (org-element-parse-buffer 'element)))
     (org-element-map ast 'clock
       (lambda (x)
         (let ((val (org-element-property :value x)))
       `(,(calendar-month-name (org-element-property :month-start val) t)
         ,(number-to-string (org-element-property :day-start val))
         ,(concat (number-to-string (org-element-property :hour-start val))
              ":"
              (format "%02d" (org-element-property :minute-start val)))
         ,(concat (number-to-string (org-element-property :hour-end val))
              ":"
              (format "%02d" (org-element-property :minute-end val)))
         ,(org-element-property :duration x)
         ,@(org-element-map
           (org-element-property :parent (org-element-property :parent x))
           'headline
             (lambda (x) (org-element-property :title x))))))))
   '(hline)
   `(("" "" "" "" "" ":=vsum(@1..@-1);T" ""))
  )
#+END_SRC

#+RESULTS: timesheet_table
| month | day | clockin | clockout |          duration | title       |
|-------+-----+---------+----------+-------------------+-------------|
| Feb   |   4 |   14:40 |    15:50 |              1:10 | First task  |
| Feb   |   2 |   23:30 |     2:00 |              2:30 | First task  |
| Feb   |   5 |   16:00 |    17:00 |              1:00 | Second task |
| Feb   |   4 |   13:00 |    14:30 |              1:30 | Third task  |
|-------+-----+---------+----------+-------------------+-------------|
|       |     |         |          | :=vsum(@1..@-1);T |             |
{% endhighlight %}

Now, if you `TAB` over the formula (or `M-e` or something similar), you get the sum
of hours in its place.

One more thing: the table could be sorted by time, so it would be more logical. What I did at
first was to add a timestamp as a first column, and use `org-sort` to sort all rows according
to this column. To do this, you can get the `:raw-value` property for each entry, which is the
calendar range in the org format, and use `split-string` to split it by the hyphens
(`--`), and take the start time:

{% highlight none %}
#+NAME: timesheet_table
#+BEGIN_SRC elisp :results table
  (nconc
   '(("timestamp" "month" "day" "clockin" "clockout" "duration" "title"))
   '(hline)
   (let ((ast (org-element-parse-buffer 'element)))
     (org-element-map ast 'clock
       (lambda (x)
         (let ((val (org-element-property :value x)))
       `(,(car (split-string (org-element-property :raw-value val) "--"))
         ,(calendar-month-name (org-element-property :month-start val) t)
         ,(number-to-string (org-element-property :day-start val))
         ,(concat (number-to-string (org-element-property :hour-start val))
              ":"
              (format "%02d" (org-element-property :minute-start val)))
         ,(concat (number-to-string (org-element-property :hour-end val))
              ":"
              (format "%02d" (org-element-property :minute-end val)))
         ,(org-element-property :duration x)
         ,@(org-element-map
               (org-element-property :parent (org-element-property :parent x))
               'headline
             (lambda (x) (org-element-property :title x))))))))
   '(hline)
   `(("" "" "" "" "" ":=vsum(@1..@-1);T" ""))
  )
#+END_SRC

#+RESULTS: timesheet_table
| timestamp              | month | day | clockin | clockout |          duration | title       |
|------------------------+-------+-----+---------+----------+-------------------+-------------|
| [2017-02-04 Sat 14:40] | Feb   |   4 |   14:40 |    15:50 |              1:10 | First task  |
| [2017-02-02 Thu 23:30] | Feb   |   2 |   23:30 |     2:00 |              2:30 | First task  |
| [2017-02-05 Sun 16:00] | Feb   |   5 |   16:00 |    17:00 |              1:00 | Second task |
| [2017-02-04 Sat 13:00] | Feb   |   4 |   13:00 |    14:30 |              1:30 | Third task  |
|------------------------+-------+-----+---------+----------+-------------------+-------------|
|                        |       |     |         |          | :=vsum(@1..@-1);T |             |
{% endhighlight %}

Then, go over an entry in the first column and do `M-x org-sort RET t` and voila! This can also
be done programmatically. Being in a rush to create the report, I didn't have time to find a way
to do this in lisp, so I used `org-babel`'s feature which enables a code block to take the
output of another code block as an input. So, I of course did it in python!


{% highlight none %}
#+NAME: timesheet_table
#+begin_src elisp :results table
  (nconc
   (let ((ast (org-element-parse-buffer 'element)))
     (org-element-map ast 'clock
       (lambda (x)
         (let ((val (org-element-property :value x)))
       `(,(car (split-string (org-element-property :raw-value val) "--"))
         ,(calendar-month-name (org-element-property :month-start val) t)
         ,(number-to-string (org-element-property :day-start val))
         ,(concat (number-to-string (org-element-property :hour-start val))
              ":"
              (format "%02d" (org-element-property :minute-start val)))
         ,(concat (number-to-string (org-element-property :hour-end val))
              ":"
              (format "%02d" (org-element-property :minute-end val)))
         ,(org-element-property :duration x)
         ,@(org-element-map
           (org-element-property :parent (org-element-property :parent x))
           'headline
             (lambda (x) (org-element-property :title x))))))))
  )
#+end_src

#+NAME: timesheet_final
#+BEGIN_SRC python :results value :var tbl=timesheet_table
  from datetime import datetime

  headers = ['*year*', '*month*', '*day*', '*clockin*', '*clockout*', '*duration*', '*title*']
  timeformat = '[%Y-%m-%d %a %H:%M]'

  for row in tbl:
      row[0] = datetime.strptime(row[0], timeformat)
      row.insert(1, row[0].year)

  tbl = sorted(tbl, key=lambda row: row[0])

  return [headers] + [None] + [row[1:] for row in tbl] + [None] + [['', '', '', '', '*total:*', ':=vsum(@2..@-1);T', '']]
#+END_SRC
{% endhighlight %}

By only evaluating the python block (`C-c C-c`), you get the prettiest table ever:


{% highlight none %}
#+RESULTS: timesheet_final
| *year* | *month* | *day* | *clockin* | *clockout* |        *duration* | *title*     |
|--------+---------+-------+-----------+------------+-------------------+-------------|
|   2017 | Feb     |     2 |     23:30 |       2:00 |              2:30 | First task  |
|   2017 | Feb     |     4 |     13:00 |      14:30 |              1:30 | Third task  |
|   2017 | Feb     |     4 |     14:40 |      15:50 |              1:10 | First task  |
|   2017 | Feb     |     5 |     16:00 |      17:00 |              1:00 | Second task |
|--------+---------+-------+-----------+------------+-------------------+-------------|
|        |         |       |           |   *total:* | :=vsum(@2..@-1);T |             |
{% endhighlight %}

Now that's nice. And it can be exported to A LOT of formats with `org-table-export`.

[docs-backquote]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html

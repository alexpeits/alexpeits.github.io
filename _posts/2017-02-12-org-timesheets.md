---
layout: post
title: "Formatting timesheet tables in org-mode"
categories: programming
tags: emacs
---

{:.note}
This post is originally adapted from [this](https://emacs.stackexchange.com/questions/23808/how-to-plot-summaries-of-timestamps-of-different-projects-clocking-in-and-out/23862#23862)
stackoverflow answer.

Org-mode is a great productivity tool, and has a lot of functionality out of
the box, including utilities for tracking time spent on a task/project. For
example, by pressing `C-c C-x C-i` on a heading, a timer is added (clock-in),
and by pressing `C-c C-x C-o` the timer stops (clock-out). Org-mode also
supports adding a report with `org-clock-report` (`C-c C-x C-r`), but, even
with the customization it offers, it is not ideal for having a fully custom
report and be able to export it to csv and whatnot.

Fortunately, newer versions of org-mode come with a utility called
*org-element*, which adds the ability to parse org-mode buffers as trees. This
proves really helpful to take all `clock` elements from a buffer and put them
in a table.

To do this, we use the following elisp code block in the respective buffer:

{% highlight none %}
#+BEGIN_SRC elisp
  (nconc
   '(("date" "project" "hours" "task"))
   '(hline)
   (let ((ast (org-element-parse-buffer 'element)))
     (org-element-map ast 'clock
       (lambda (x)
         (let* ((val (org-element-property :value x))
                (task (org-element-property :parent (org-element-property :parent x))))
           `(,(let ((year (org-element-property :year-start val))
                    (month (calendar-month-name
                            (org-element-property :month-start val)))
                    (day (org-element-property :day-start val)))
                ;; (insert (org-element-property :raw-value val))
                (format "%s %s, %s" month day year))
             ,(org-element-property :PROJECT task)
             ,(org-element-property :duration x)
             ,(org-element-property :title task)
             )))))
   '(hline)
   '(("" "total:" ":=vsum(@2..@-1);T" "")))
#+END_SRC
{% endhighlight %}

What this code block does is parse the buffer, and map the lambda function over all of the `clock` elements
in the buffer. For each element, we get its value (which is the datetime range), and its header, which contains
some properties, such as the project name. The name of the task is the header name.

By prepending a list, we can also add headers. At the end, a `calc` formula can be used for adding the times
to get a total. This formula sums the current column (hence the empty strings before and after), from the
second row (not including the header), up to the second to last row (not including the formula itself). The
`;T` after the formula instructs it to add time, rather than numbers. Finally, the `hline` function inserts
a dashed line for making the table more presentable.

Suppose the buffer looks like this:

{% highlight none %}
#+BEGIN_SRC 
  * project 1

  ** Task 1
    :PROPERTIES:
    :PROJECT:  project_1
    :END:
    CLOCK: [2017-06-24 Sat 19:15]--[2017-06-24 Sat 22:05] =>  2:50


  ** Task 2
    :PROPERTIES:
    :PROJECT:  project_1
    :END:
    CLOCK: [2017-06-28 Wed 00:35]--[2017-06-28 Wed 03:35] =>  3:00
    CLOCK: [2017-06-27 Tue 16:55]--[2017-06-27 Tue 19:00] =>  2:05
    CLOCK: [2017-06-26 Mon 21:30]--[2017-06-27 Tue 01:00] =>  3:30

  * project 2

  ** Task 1
    :PROPERTIES:
    :PROJECT:  project_2
    :END:
    CLOCK: [2017-06-29 Thu 15:18]--[2017-06-29 Thu 18:18] =>  3:00
#+END_SRC
{% endhighlight %}

If the above code is wrapped in an `elisp` block and executed (`C-c C-c`), it outputs the
following table:

{% highlight none %}
#+BEGIN_SRC 
  | date          | project   |             hours | task   |
  |---------------+-----------+-------------------+--------|
  | June 24, 2017 | project_1 |              2:50 | Task 1 |
  | June 28, 2017 | project_1 |              3:00 | Task 2 |
  | June 27, 2017 | project_1 |              2:05 | Task 2 |
  | June 26, 2017 | project_1 |              3:30 | Task 2 |
  | June 29, 2017 | project_2 |              3:00 | Task 1 |
  |---------------+-----------+-------------------+--------|
  |               | total:    | :=vsum(@2..@-1);T |        |
#+END_SRC
{% endhighlight %}

To run the formula, press `TAB` while in its cell.

Afterwards, the table can be exported to csv with `org-table-export`.

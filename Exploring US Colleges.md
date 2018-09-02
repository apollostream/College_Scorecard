# Introduction

How'd you like to explore the landscape of 4-year colleges in the USA? If your answer is "Yes", here's the Bayesian-infused solution I created to do just that: **["Best Colleges for You"](https://thompsonml.shinyapps.io/BestCollegeApp/" target="_blank)**.
![A virtual college roadtrip exploring colleges ranked just for you.](BestCollegeApp_roadtrip1.png)

## Motivation

My family and I answered that question a couple of years ago with an enthusiastic "We'd love to!"  Not because we have some great love for college exploration.  But, because our daughter would soon be applying to colleges.  Class of '16, she was excited about all the possibilities that college offered.  

In her case, she had earned recognition as a National Merit Finalist, so she rightfully thought the sky's the limit.  But what if her dream school didn't accept her?  What was going to be her Plan B?  What if she wasn't so sure she really wanted to major in a STEM (science, technology, engineering, math) field but instead wanted to pursue her music and art -- all of at which she showed proficiency?  What about locale, region? Classmates, roommates, professors, campus ambience, etc. etc. etc?  And costs?

And then ... where to start? Soon, excitement risked fading into disillusionment ... bewilderment ... anxiety<sup id="a1">[1](#f1)</sup> ....


![by Idea Trader/Shutterstock.com](https://github.com/apollostream/College_Scorecard/blob/master/shutterstock_701062771.jpg) <!-- .element height="50%" width="50%" -->

## Solutions

I think early in the college search process, it's helpful to have a tool that can generate a ranked list of colleges that is customized for the student.  The typical free online resources were far too generic.  As template-driven filters, online tools just screen out everything not in the particular region, size or cost range you specify. Instead I set out to provide us with some level of customized prioritization -- importance weighting, if you will -- of the criteria driving our selection.  

Thankfully, just as we began the college search process, the U.S. Department of Education released its [College Scorecard](https://collegescorecard.ed.gov/data/) dataset.  Better yet, [Kaggle](https://kaggle.com) had taken the dataset, made it readily useful, and hosted an open "swag" posting that [College Scorecard dataset](https://collegescorecard.ed.gov/data/).  And the *coup de grace*, I'm good at math modeling, especially Bayesian analysis, & know my way around data ([see me...](https://www.linkedin.com/in/mlthomps/)).

So, I entered the Kaggle contest, won a nod as ["Script of the Week" for January 8, 2016](http://blog.kaggle.com/2016/02/19/december-2015-january-2016-scripts-of-the-week/), and got a Kaggle t-shirt out of it!

![Reward for Kaggle.com "Script of the Week", January 8, 2016](http://github.com/apollostream/College_Scorecard/blob/master/kaggle_tshirt.jpg)

Encouraged by this outcome and looking to build an easy-to-use tool, I took things a step further and created a web-based R Shiny app to help us explore the landscape of 4-year colleges in the USA: ["Best Colleges for You"](https://thompsonml.shinyapps.io/BestCollegeApp/)!

## What of It?

In this post, I'll give you a quick run through of the "Best Colleges for You" app to give you an idea of what you might do with it.  Hopefully, the different college rankings it generates just for you will surface some college options you hadn't entertained yet and spark your imagination to explore further afield for a college that's right for you.

Also, for the data scientists out there, this post gives a brief description of the Bayesian bits and discrete choice modeling-motivated engine that drives the customized ranked lists that the app generates.

# Getting Started

<b id="f1">1.</b> Image by Idea Trader/Shutterstock.com. [↩](#a1)


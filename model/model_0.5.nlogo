;; Model V0.5: complex function for preference, with placeholder code for considering both neighbour behaviours and opinions

globals [
 p-set ;; set of probability used to calculate total probability of evidence
]

turtles-own [
  valuation-wwoh  ;; private beliefs about the WWOH behaviour, either 0 or 1
  valuation-norm  ;; beliefs about how many other agents support WWOH behaviour, a real number between 0 and 1
  preference-wwoh   ;; preference about the WWOH behaviour, either 0 or 1
  behaviour-wwoh  ;; actual WWOH behaviour, either 0 or 1

  neighbour-valuation ;; beliefs of accessed neighbours
  neighbour-behaviour ;; behaviours of accessed neighbours

  ;; Bayesian updating from communicated beliefs
  p-H-com ;; prior
  p-E-H-com ;; conditional probability of the evidence
  p-E-com ;; total probability of the evidence
  p-H-E-com ;; posterior belief

  ;; Bayesian updating from observed behaviours
  p-H-obs ;; prior
  p-E-H-obs ;; conditional probability of the evidence
  p-E-obs ;; total probability of the evidence
  p-H-E-obs ;; posterior belief
]

;##############################
;####                      ####
;####   Setup Procedures   ####
;####                      ####
;##############################

to setup
  clear-all
  setup-patches
  setup-turtles

  ;; set the set p for calculating total probability
  set p-set ( range 0 1 0.1 )
  set p-set lput 1 p-set

  reset-ticks
end

to setup-patches
  ask patches [ set pcolor black ]
end

to setup-turtles
  ;; set up scale-free or redos-renyi network
  ifelse scale-free = TRUE [ set-scale-free ][ set-erdos-renyi ]

  ;; set turtle and link characteristics
  set-turtle-characteristics
  set-link-characteristics

end

to set-scale-free  ;; adapted from Fränken & Pilditch (2021)
  ;; make the initial network of two turtles and an edge
  make-node nobody                     ;; first node, unattached
  make-node turtle 0                   ;; second node, attached to first node
  let SF-create-count 2
  while [SF-create-count < n-agents]   ;; loop to generate scale-free network
   [ make-node find-partner
     layout
     ask links [ set color gray ]
     set SF-create-count (SF-create-count + 1)
   ]
end


to set-erdos-renyi  ;; adapted from Wilensky, U. & Rand, W. (2015)
  ;; create turtles and layout into a circle
  create-turtles n-agents [
    layout-circle turtles (max-pxcor - 5)
  ]
  ;; Now give each pair of turtles an equal chance
  ;; of creating a link
  ask turtles [
    ;; we use "self > myself" here so that each pair of turtles is only considered once
    create-links-with turtles with [ self > myself and
                                     random-float 1.0 < density ]
  ]
  layout  ;; further layout for visual purpose
end

to set-turtle-characteristics
  ask turtles [
    ;; private beliefs about WWOH
    set valuation-wwoh 0
    if (random-float 1.0 < p-supporter) [ set valuation-wwoh 1 ]

    ;; beliefs about the WWOH norm
    ;; to start with, we use uniform distributions
    set valuation-norm 0
    ifelse (random-float 1.0 < p-underestimate) [
      set valuation-norm random-float p-supporter ] [
      set valuation-norm ( p-supporter + random-float ( 1 - p-supporter ) )
    ]

    ;; set priors
    set p-H-com random-normal prior-mu prior-sigma
    while [ p-H-com > 1 or p-H-com < 0 ][
      set p-H-com random-normal prior-mu prior-sigma
      set p-H-obs (valuation-norm - ( 1 - p-beh ) * p-H-com) / p-beh

      while [  p-H-obs > 1 or p-H-obs < 0 ][
        set p-H-com random-normal prior-mu prior-sigma
        set p-H-obs (valuation-norm - ( 1 - p-beh ) * p-H-com) / p-beh
      ]
    ]

  ]

  ;; WWOH behaviour, only demonstrated by those surpporting WWOH
  ask turtles [ set behaviour-wwoh 0 ]
  let n-supporter p-wwoh * n-agents
  ask n-of n-supporter turtles with [ valuation-wwoh = 1 ] [ set behaviour-wwoh 1 ]

  ;; WWOH preference, those demonstrating behaviours are set just above the threshold
  ;; othersie, just below the threshold
  ask turtles with [ behaviour-wwoh = 0 ] [ set preference-wwoh 0.5 - 0.01 ]
  ask turtles with [ behaviour-wwoh = 1 ] [ set preference-wwoh 0.5 + 0.01 ]

  ;; set appearance
  ask turtles [
    ifelse (behaviour-wwoh = 1) [ set color red ] [ set color grey ]
    set shape "circle"
    set size .5

  ]
end

to set-link-characteristics
  ask links [ set color grey - 1
              set thickness 0.1 ]
end



;###########################
;####                   ####
;####   Go Procedures   ####
;####                   ####
;###########################

to go
  ask turtles [
    update-wwoh-behaviour
  ]
end


to update-wwoh-behaviour
  ;; get access to the beliefs and behaviours of certian number of neighbours
  let n-com-neighbour round ( p-com * [ count link-neighbors ] of self )
  let n-obs-neighbour round ( p-obs * [ count link-neighbors ] of self )
  set neighbour-valuation [ valuation-wwoh ] of n-of n-com-neighbour link-neighbors
  set neighbour-behaviour [ behaviour-wwoh ] of n-of n-obs-neighbour link-neighbors

  ;; update agents' belief about the norm
  ;; neighbours' belief and behaviours are evaluated separately and then linearly combined
  let x-com sum neighbour-valuation
  set p-E-H-com binomial p-E-H-com n-com-neighbour x-com  ;; conditional probs of evidence
  set p-E-com calculate-p-E n-com-neighbour x-com  ;; total probability of evidence
  set p-H-E-com p-H-com * p-E-H-com / p-E-com  ;; update based on Bayes' rule
  ;; limit the range of p-H to [0, 1]
  if ( p-H-E-com > 1 ) [ set p-H-E-com 1 ]

  let x-obs sum neighbour-behaviour
  set p-E-H-obs binomial p-E-H-obs n-obs-neighbour x-obs  ;; conditional probs of evidence
  set p-E-obs calculate-p-E n-obs-neighbour x-obs  ;; total probability of evidence
  set p-H-E-obs p-H-obs * p-E-H-obs / p-E-obs  ;; update based on Bayes' rule
  ;; limit the range of p-H to [0, 1]
  if ( p-H-E-obs > 1 ) [ set p-H-E-obs 1 ]

  ;; combine communicated beliefs and observed behaviours
  set valuation-norm ( 1 - p-beh ) * p-H-E-com + p-beh * p-H-E-obs

  ;; update agents' wwoh preference based on the belief about the norm
  set preference-wwoh ( 1 / ( 1 + e ^ (- k * ( valuation-norm - r ) ) ) ) * ( 1 + b * ( 2 * valuation-wwoh - 1 ))

  ;; update agents' behaviour based on their decision
  if preference-wwoh > 0.5 [ set behaviour-wwoh 1 ]
  if preference-wwoh < 0.5 [ set behaviour-wwoh 0 ]
  if preference-wwoh = 0.5 [
    ifelse random-float 1.0 < 0.5 [ set behaviour-wwoh 1 ] [ set behaviour-wwoh 0 ]
  ]
  ifelse (behaviour-wwoh = 1) [ set color red ] [ set color grey ]
end

to-report binomial [ p n x ]
  let prob ( choose n x ) * ( p ^ x ) * ( ( 1 - p ) ^ ( n - x ) )
  report prob
end

to-report calculate-p-E [ n x ]
  let p-set-size ( length p-set )
  let i 0
  let prob 0
  while [ i < p-set-size - 1 ] [
    let p item i p-set
    set prob ( prob + p * ( binomial p n x ) )
    set i ( i + 1 )
  ]
  report prob
end

to-report choose [ n x ]  ;; customized functional to calculate combinatorics
  report ( stepwisefactorial2 n x ) / ( stepwisefactorial1 (n - x) )
end

;#################################################
;####                                         ####
;####   Functions Adapted From Other Models   ####
;####                                         ####
;#################################################

;; 1. Wilensky, U. (2005).  NetLogo Preferential Attachment model.  http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

to make-node [old-node]
  create-turtles 1
  [
    set color red
    if old-node != nobody
      [ create-link-with old-node [ set color green ]
        ;; position the new node near its partner
        move-to old-node
        fd 8
      ]
  ]
end

;; This code is the heart of the "preferential attachment" mechanism
to-report find-partner
  report [one-of both-ends] of one-of links
end

;=============
;   Layout
;=============

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end


;; 2. Fränken & Pilditch (2021). Cascades Across Networks are Sufficient for the Formation of Echo Chambers: An Agent-Based Model
;; See the "Setup Procedures" section

;; 3. Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.
;; See the "Setup Procedures" section

;; 4. https://stackoverflow.com/questions/32657925/netlogo-computing-beta-distribution-function

to-report stepwisefactorial2 [n d]
  if (n = 0) [report (1 / (stepwisefactorial1 (d)))]
  if (d = 0) [report stepwisefactorial1 (n)]
  report ((n / d)* stepwisefactorial2 (n - 1) (d - 1))
end

to-report stepwisefactorial1 [d]
  if d = 0 [ report 1 ]
  report d * stepwisefactorial1 (d - 1)
end
@#$#@#$#@
GRAPHICS-WINDOW
909
79
1280
451
-1
-1
3.0
1
10
1
1
1
0
0
0
1
-60
60
-60
60
0
0
1
ticks
30.0

BUTTON
44
38
110
71
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
161
104
269
137
scale-free
scale-free
0
1
-1000

SLIDER
42
104
151
137
n-agents
n-agents
10
1000
500.0
1
1
NIL
HORIZONTAL

SLIDER
281
103
378
136
density
density
0.01
1
0.01
0.01
1
NIL
HORIZONTAL

TEXTBOX
162
77
276
105
Erdos-Renyi vs. scale-free network
9
0.0
1

TEXTBOX
282
82
432
100
ER network density
9
0.0
1

SLIDER
536
46
678
79
p-supporter
p-supporter
0
1
0.8
0.01
1
NIL
HORIZONTAL

TEXTBOX
426
46
531
74
probabiliy supporting WWOH
9
0.0
1

SLIDER
536
79
678
112
p-underestimate
p-underestimate
0
1
0.8
0.01
1
NIL
HORIZONTAL

TEXTBOX
425
79
539
114
probability underestimating supporters
9
0.0
1

SLIDER
536
112
678
145
p-wwoh
p-wwoh
0
1
0.05
0.01
1
NIL
HORIZONTAL

TEXTBOX
425
125
606
153
probability of WWOH 
9
0.0
1

PLOT
43
417
243
567
valuation-wwoh
NIL
NIL
0.0
2.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ valuation-wwoh ] of turtles"

PLOT
242
417
442
567
valuation-norm
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [ valuation-norm ] of turtles"

PLOT
442
417
642
567
behaviour-wwoh
NIL
NIL
0.0
2.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ behaviour-wwoh ] of turtles "

MONITOR
95
569
185
614
p-supporter
(count turtles with [ valuation-wwoh = 1 ]) / n-agents
17
1
11

MONITOR
292
569
409
614
p-underestimate
(count turtles with [ valuation-norm <  mean [ valuation-wwoh] of turtles ]) / n-agents
17
1
11

MONITOR
521
568
584
613
p-wwoh
(count turtles with [behaviour-wwoh = 1]) / n-agents
17
1
11

SLIDER
176
171
317
204
p-obs
p-obs
0.01
1
0.5
0.01
1
NIL
HORIZONTAL

TEXTBOX
44
170
194
198
prop. of neighbours being accessed behaviours
9
0.0
1

SLIDER
176
204
317
237
p-com
p-com
0.01
1
0.5
0.01
1
NIL
HORIZONTAL

TEXTBOX
44
209
194
231
prop. of neighbours being accessed beliefs
9
0.0
1

BUTTON
124
38
205
71
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
45
300
195
318
resistance to norm
9
0.0
1

TEXTBOX
44
326
164
350
sensitivity of preference to norm
9
0.0
1

SLIDER
174
292
315
325
r
r
0
1
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
174
325
315
358
k
k
1
20
10.0
1
1
NIL
HORIZONTAL

TEXTBOX
44
367
167
395
impact of beleifs about wwoh on preference
9
0.0
1

SLIDER
174
358
315
391
b
b
0
1
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
219
40
282
73
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
676
422
876
572
plot 1
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [ p-H-com ] of turtles"

TEXTBOX
44
242
182
275
prop. of neighbour behaviours in evaluating norm
9
0.0
1

SLIDER
176
237
317
270
p-beh
p-beh
0
1
0.5
0.01
1
NIL
HORIZONTAL

TEXTBOX
364
176
495
198
mean of neighbour beliefs priors
9
0.0
1

TEXTBOX
362
212
500
240
sd of neighbour beliefs priors
9
0.0
1

SLIDER
494
172
637
205
prior-mu
prior-mu
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
494
204
637
237
prior-sigma
prior-sigma
0
1
0.2
0.1
1
NIL
HORIZONTAL

PLOT
938
484
1138
634
plot 2
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [p-H-obs] of turtles"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

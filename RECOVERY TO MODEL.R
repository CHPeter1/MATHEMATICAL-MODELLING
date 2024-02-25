
##Increasing complexity
#To demonstrate increasing complexity, let us now develop a model 
#for a disease wherein three compartments viz susceptible, infected, 
#and recovered are present. There are no births (inflows) or deaths (outflows)
#in the model. Further, all those who are infected are also infectious and once recovered, 
#immunity is long lasting. The basic assumptions of compartment models such as homogenity 
#among others are also applicable.

##Visualization of the model
grViz("digraph flowchart {
      graph [layout = dot,
       rankdir = LR]
      node [fontname = Helvetica, shape = oval,]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3'] 
      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      }

      [1]: 'Susceptible'
      [2]: 'Infected'
      [3]: 'Recovered'
      ")





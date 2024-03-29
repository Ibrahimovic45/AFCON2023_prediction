# -*- coding: utf-8 -*-
"""plot.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1ifakPzW8S9uCdaNubEvTfdJErRbk3FEI
"""

import networkx as nx
#from networkx.drawing.nx_pydot import graphviz_layout
from networkx.drawing.nx_agraph import graphviz_layout
import matplotlib.pyplot as plt

def bracket():
  plt.figure(figsize=(15, 10))
  G = nx.balanced_tree(2, 3)


  labels = []
  result = afcon_sim()
  playoffs = result[4]

  for p in playoffs.keys():
      for game in playoffs[p]:
          label = f"{game[0]}({round(game[2][0], 2)}) \n {game[1]}({round(game[2][1], 2)})"
          labels.append(label)

  labels_dict = {}
  labels_rev = list(reversed(labels))

  for l in range(len(list(G.nodes))):
      labels_dict[l] = labels_rev[l]

  pos = graphviz_layout(G, prog='twopi')
  labels_pos = {n: (k[0], k[1]-0.08*k[1]) for n,k in pos.items()}
  center  = pd.DataFrame(pos).mean(axis=1).mean()


  nx.draw(G, pos = pos, with_labels=False, node_color=range(15), edge_color="#bbf5bb", width=10, font_weight='bold',cmap=plt.cm.Greens, node_size=5000)
  nx.draw_networkx_labels(G, pos = labels_pos, bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="black", lw=.5, alpha=1),
                          labels=labels_dict)
  texts = ["Round \nof 16", "Quarter \n Final", "Semi \n Final","Final\n"]
  pos_y = pos[0][1] + 55
  for text in reversed(texts):
      pos_x = center -10
      pos_y -= 65
      plt.text(pos_y, pos_x, text, fontsize = 14)

  plt.axis('equal')
  return plt.show()
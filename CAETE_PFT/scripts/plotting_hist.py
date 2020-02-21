import matplotlib.pyplot as plt
import pandas as pd

table_traits = input('which file do you want to see? ')

traits = pd.read_csv(table_traits) 
chosen_trait = input('which trait do you want to see? ')
plt.hist(traits[chosen_trait], bins= 300, alpha =0.5)
plt.show()

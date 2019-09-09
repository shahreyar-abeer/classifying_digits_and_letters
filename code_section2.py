
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import random
import glob


# In[2]:


# changing the directory to point to the data
get_ipython().magic('cd C:\\Users\\Zauad\\Google Drive\\Projects\\500$\\Initial Docs\\csv docs')


# In[3]:


get_ipython().magic('pwd ()')


# In[4]:


# list of characters
characters = ['1', '3', '4', '5', '7', 'a', 'b', 'c', 'd', 'e']


# ### At first, we read all the images in a single list called *all_images*. The list contains 10 elements, one for each character, each of which contains 10 16x16 images. So if we want the third image of the first character, we call it by all_images[0][2]

# In[5]:


# a function that reads the 10 images of each character and returns 10 16x16 arrays.
def read_and_make_array(image):
    files = glob.glob('{}*.csv'.format(image))
    nd = np.ndarray([10, 16, 16], dtype = int)
    for i in [0, 2, 3, 4, 5, 6, 7, 8, 9, 1]:
        file = pd.read_csv('./{}'.format(files[i]))
        nd[i] = file.iloc[0:, 1:]
    return nd


# In[6]:


# reading all the images in a single list
all_images = [np.ndarray([10, 16, 16], dtype = int)] * 10
for i in range(len(characters)):
    all_images[i] = read_and_make_array(characters[i])


# ### Below are functions to calculate the features

# In[7]:


# some features

def nr_pix(image):
    return (image == 1).sum()

def height(image):
    return sum(image.sum(axis = 1) > 0)

def width(image):
    return sum(image.sum(axis = 0) > 0)

def rows_with_1(image):
    return  (image.sum(axis = 1) == 1).sum()

def cols_with_1(image):
    return  (image.sum(axis = 0) == 1).sum()

def rows_with_5_plus(image):
    return (image.sum(axis = 1) > 5).sum()

def cols_with_5_plus(image):
    return  (image.sum(axis = 0) > 5).sum()


# ### The codes below calculates the neighbours of a pixel and then the features related to neighbours.

# In[20]:


# the upper-left neighbour is indexed 0, going clockwise, upper would be 1, upper-right 2 and so on.
grid = np.array([[0, 1, 2], [7, 'P', 3], [6, 5, 4]])
grid
# this is how the grid looks like, p being the pixel.


# In[21]:


# function that gets the neighbours of a single pixel
def get_neighbours_single(image, i, j):
    n = np.zeros(8)
    if i > 0:
        if j > 0:
            n[0] = image[i - 1, j - 1]
        n[1] = image[i - 1, j]
        if j < 15:
            n[2] = image[i - 1, j + 1]
    if j < 15:
        n[3] = image[i, j + 1]
        if i < 15:
            n[4] = image[i + 1, j + 1]
    if i < 15:
        n[5] = image[i + 1, j]
        if j > 0:
            n[6] = image[i + 1, j - 1]
    if j > 0:
        n[7] = image[i, j - 1]
    return n

# function to get neighbours of all pixels of an image.
def neighbours_array(image):
    neighbours = np.ndarray([256, 8], dtype = int)
    k = 0
    l = []
    for i in range(image.shape[0]):
        for j in range(image.shape[1]):
            if image[i, j] == 1:
                neighbours[k] = get_neighbours_single(image, i, j)
                l.append(k)
            k += 1
    return neighbours[l]


# In[22]:


# some more features involving neighbours

def neighbours_1(image):
    n = neighbours_array(image)
    return (n.sum(axis = 1) == 1).sum()

def neighbours_3_or_more(image):
    n = neighbours_array(image)
    return (n.sum(axis = 1) >= 3).sum()

below = [4, 5, 6]
above = [0, 1, 2]
before = [0, 7, 6]
after = [2, 3, 4]

def none_below(image):
    n = neighbours_array(image)
    return (n[:, below].sum(axis = 1) == 0).sum()

def none_above(image):
    n = neighbours_array(image)
    return (n[:, above].sum(axis = 1) == 0).sum()

def none_after(image):
    n = neighbours_array(image)
    return (n[:, after].sum(axis = 1) == 0).sum()

def none_before(image):
    n = neighbours_array(image)
    return (n[:, before].sum(axis = 1) == 0).sum()


# ### These codes calculate the *bd* and *custom* feature.

# In[ ]:


def column_with_highest_black(image):
    m = np.where(image.sum(axis = 0) == image.sum(axis = 0).max())
    return m[0][0]

def black_to_right_of_highest_col(image):
    m = column_with_highest_black(image)
    cols = image.sum(axis = 0)
    return cols[(m+1):].sum()

def black_to_left_of_highest_col(image):
    m = column_with_highest_black(image)
    cols = image.sum(axis = 0)
    return cols[:m].sum()

def bd(image):
    return black_to_right_of_highest_col(image) - black_to_left_of_highest_col(image)


# In[23]:


def density(image):
    return height(image) * width(image) / nr_pix(image)


# ### Creating the data frame of features

# In[32]:


column_names = ['label', 'nr_pix', 'height', 'width', 'tallness', 'rows_with_1', 'cols_with_1', 'rows_with_5_plus', 'cols_with_5_plus', 
          'neigh_1', 'neigh_3_plus', 'none_below', 'none_above', 'none_before', 'none_after', 'nr_eyes', 'bd', 'custom']


# In[33]:


# the initial dataset
data = pd.DataFrame(index = range(100), columns = column_names)


# In[34]:


# creating the data frame of features.
i = 0
c = 0
for each_10 in all_images:
    for image in each_10:
        data.iloc[i, 0] = characters[c]
        data.iloc[i, 1] = nr_pix(image)
        data.iloc[i, 2] = height(image)
        data.iloc[i, 3] = width(image)
        data.iloc[i, 4] = height(image)/ width(image)
        data.iloc[i, 5] = rows_with_1(image)
        data.iloc[i, 6] = cols_with_1(image)
        data.iloc[i, 7] = rows_with_5_plus(image)
        data.iloc[i, 8] = cols_with_5_plus(image)
        data.iloc[i, 9] = neighbours_1(image)
        data.iloc[i, 10] = neighbours_3_or_more(image)
        data.iloc[i, 11] = none_below(image)
        data.iloc[i, 12] = none_above(image)
        data.iloc[i, 13] = none_before(image)
        data.iloc[i, 14] = none_after(image)
        data.iloc[i, 15] = random.randint(0, 5)
        data.iloc[i, 16] = bd(image)
        data.iloc[i, 17] = density(image)
        i += 1
    c += 1


# In[ ]:


data.to_csv('./features.csv', index = False)


# In[48]:


data.head(10)


# In[49]:


data.tail(10)


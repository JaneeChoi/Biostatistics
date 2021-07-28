import torch
import numpy as np
import torch.nn as nn
import torch.optim as optim
import torchvision.utils as utils
import torchvision.datasets as dsets
import torchvision.transforms as transforms
from collections import OrderedDict
import torch.nn.functional as F
from sklearn.model_selection import train_test_split
from torch.autograd import Variable
from torch.utils.data import Dataset
from torch.utils.data import DataLoader
from sklearn.metrics import roc_auc_score
is_cuda = torch.cuda.is_available()
device = torch.device('cuda' if is_cuda else 'cpu')
import pandas as pd
import multiprocessing
import sys, os

class CustomDataset(Dataset): 
  def __init__(self, DATA, inputlength):
    self.x_data = DATA[:,0:(inputlength-1)]
    self.y_data = DATA[:,(inputlength-1)].reshape(-1,1)
  def __len__(self): 
    return len(self.x_data)
  def __getitem__(self, idx): 
    x = torch.FloatTensor(self.x_data[idx])
    y = torch.FloatTensor(self.y_data[idx])
    return x, y
  
class Net_conv_4(nn.Module):
  def __init__(self,  nvar, nconv, device):
    super(Net_conv_4, self).__init__()
    self.nvar = nvar
    self.fc1 = nn.ModuleList([nn.Linear(nvar[i],nconv[i], bias = False) for i in range(len(self.nvar))])
    self.fc4 = nn.ModuleList([nn.Linear(nconv[i], 1, bias = False) for i in range(len(self.nvar))])
    self.fc1_bn = nn.BatchNorm1d(len(nvar))
    self.fc2=nn.Linear(len(nvar),1)
    self.device = device
  def forward(self, x):
    kk=0
    s=list()
    for i, l in enumerate(self.fc1):
      k=kk
      kk=kk+self.nvar[i]
      t= x[:,k:kk]
      s.append(torch.sigmoid(l(t))) 
    for i, l in enumerate(self.fc4):
      if i==0:
        if nconv[i] == 1:
          x= s[i]
        else:
          x=torch.sigmoid(l(s[i]))
      else:
        if nconv[i] == 1:
          x= torch.cat((x,s[i]),1)
        else:
          x=torch.cat((x,torch.sigmoid(self.fc4[i](s[i]))),1)
    x=self.fc1_bn(x)
    x = x/(torch.norm(x,2)+0.00000001)
    x=self.fc2(x)
    x= torch.sigmoid(x)
    return(x)



best_epoch = 1
best_learning_rate = 1
best_penalty_pathHCC = 0.2
best_penalty_metpath = 0
best_nconv = [2, 1, 2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1]

for simnum in range(0,10001):
  train = 'pd.read_csv("/data/member/cwpark/liv/article_metrenew/tmpdata/perm' + str(simnum) + '.csv")'
  train = eval(train)
  Pep = 'pd.read_csv("/data/member/cwpark/liv/article_metrenew/annot_path.csv")'
  Pep = eval(Pep)            #data loading
  groupunique = list(OrderedDict.fromkeys(Pep["group"]))
  grouplen = len(groupunique)
  nvar = []
  for i in range(0,grouplen):  
    nvar.append(sum(Pep["group"]==groupunique[i]))
  mask = torch.zeros(grouplen,len(train.columns))
  kk = 0
  for i in range(0, grouplen):
    k = kk
    kk = kk+nvar[i]
    for j in  range(k,kk):
      mask[i,j] = 1
  num_epochs = 10000
  loss_limit = 0.00001
  nconv_list = [best_nconv]
  learning_rate_list = [best_learning_rate]
  penalty_list_metpath = [best_penalty_metpath]
  penalty_list_pathHCC = [best_penalty_pathHCC]
  train = torch.from_numpy(train.values).float()
  for nconv in nconv_list:
    for penalty_metpath in penalty_list_metpath:
      for learning_rate in learning_rate_list:
        for penalty_pathHCC in penalty_list_pathHCC:
          print("start")
          data_train = CustomDataset(train, (train.shape[1]))
          input_size = train.shape[1]
          pathway_size = grouplen
          num_classes = 2
          data_train = DataLoader(data_train, batch_size = train.shape[0], shuffle = True)
          model_conv_4 = Net_conv_4(nvar, nconv, device)
          optimizer_conv_4 = torch.optim.LBFGS([{'params': model_conv_4.parameters()}], lr=learning_rate, max_iter = 50000, max_eval = None, tolerance_change= 1e-07, history_size = 100, line_search_fn = "strong_wolfe")
          class_weight = torch.FloatTensor([1,1])
          criterion = nn.BCELoss()
          testAUC_conv_4= 0
          validAUC_conv_4 = 0
          count = 0
          trainAUC_conv_4 = 0
          epoch_conv_4 = 0
          loss_valid = 100000
          t=0
          x=0
          loss_before = 1000000
          for epoch in range(0,best_epoch):
            for batch_idx, samples in enumerate(data_train):
              def closure():
                optimizer_conv_4.zero_grad()
                x_train, y_train = samples
                output_conv_4_result = model_conv_4(x_train)
                output_conv_4_result = torch.squeeze(output_conv_4_result)
                y_train = torch.squeeze(y_train)
                loss_conv_4 = criterion(output_conv_4_result, y_train)
                for param in model_conv_4.fc2.parameters():
                  loss_conv_4 = loss_conv_4 + penalty_pathHCC * torch.norm(param,2) * torch.norm(param,2)
                for x in model_conv_4.fc4:
                  for param in x.parameters():
                    loss_conv_4 = loss_conv_4 + penalty_metpath * torch.norm(param,2)* torch.norm(param,2)
                for x in model_conv_4.fc1:
                  for param in x.parameters():
                    loss_conv_4 = loss_conv_4 + penalty_metpath * torch.norm(param,2)* torch.norm(param,2)     
                loss_conv_4=  loss_conv_4.type(torch.FloatTensor)
                loss_conv_4.backward()
                return(loss_conv_4)
              loss = optimizer_conv_4.step(closure)
          param = list(model_conv_4.fc2.parameters())[0]
          param = np.array(param.tolist()[0])
          param_file = "np.savetxt('/data/member/cwpark/liv/article_metrenew/conv/param" + str(simnum) + ".txt',param)"
          eval(param_file)

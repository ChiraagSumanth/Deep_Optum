from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation
from keras import backend as K
from keras.models import load_model

import h5py
import argparse
import os
import numpy as np
import csv

def data(train_file, test_file):
	X_train = []
	y_train = []
	with open(train_file) as f:
		rows = csv.reader(f, delimiter=',')
		next(rows)
		for r in rows:
			if ('-' not in r) and ('NA' not in r):
				y_train.append(r[80])
				del r[80]
				del r[0]
				X_train.append(r)

	X_test = []
	y_test = []
	with open(test_file) as f:
		rows = csv.reader(f, delimiter=',')
		next(rows)
		for r in rows:
			if ('-' not in r) and ('NA' not in r):
				y_test.append(r[80])
				del r[80]
				del r[0]
				X_test.append(r)


	y_train = [[1,0] if e == 'TRUE' else [0,1] for e in y_train]
	X_train = np.array(X_train)
	y_train = np.array(y_train)

	y_test = [[1,0] if e == 'TRUE' else [0,1] for e in y_test]
	X_test = np.array(X_test)
	y_test = np.array(y_test)

	return X_train, y_train, X_test, y_test

def train_model(X_train, y_train):

	model = Sequential()
	model.add(Dense(64, activation='relu', input_dim=X_train.shape[1]))
	model.add(Dropout(0.1))
	model.add(Dense(32, activation = 'relu'))
	model.add(Dense(2, activation = 'softmax'))

	model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

	model.fit(X_train, y_train, epochs=5, batch_size=64, 
		validation_data=(X_test, y_test))

	return model

def test_model(model, X_test, y_test, write = False):
	preds = model.predict(X_test, batch_size=64)

	y_pred = np.argmax(preds, axis = 1)
	y_true = np.array([0 if e[0] == 1 else 1 for e in y_test])

	print "High Risk Accuracy:"
	op = np.logical_or(y_true, y_pred)
	print float(len(y_pred) - np.sum(op))/np.sum(y_true == 0)

	print "Low Risk Accuracy:"
	op = np.logical_and(y_true, y_pred)
	print float(np.sum(op))/np.sum(y_true == 1)

	if (write):
		y_wp = preds[:,0]
		with open('nnet_preds.csv','w') as f:
			writer = csv.writer(f)
			writer.writerows([y_wp])

if __name__ == "__main__":

	parser = argparse.ArgumentParser(description='Deep Learning Model Non SDH')
	parser.add_argument('--mode', required=True, choices=['train', 'test'], dest='mode')
	parser.add_argument('--dir', default = '.', dest='dir')
	parser.add_argument('--train_file', default = 'hcc_train_data.csv', dest='train_f')
	parser.add_argument('--test_file', default = 'hcc_test_data.csv', dest='test_f')
	parser.add_argument('--model_path', default = 'non_sdh_trained_v2.h5', dest='model_p')
	args = parser.parse_args()
	
	train_p = os.path.join(args.dir, args.train_f)
	test_p = os.path.join(args.dir, args.test_f)
	X_train, y_train, X_test, y_test = data(train_p, test_p)
	
	if (args.mode == 'train'):
		model = train_model(X_train,y_train)
	else:
		load_p = os.path.join(args.dir, args.model_p)
		model = load_model(load_p)
	
	test_model(model, X_test, y_test, False)
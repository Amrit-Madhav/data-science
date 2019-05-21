"""
Created by AMRIT MADHAV

"""
# importing the necessary packages
import numpy as np
import argparse
import time
import cv2
import os
import sys

######################### NOTE:->  #####################################################################################################
##constructing the argument parse and parsing the arguments
##--image : The path to the input image. It will detect objects in this image using YOLO.
## The base path to the YOLO directory. My script will then load the required files from here. 
##--confidence : Minimum probability to filter weak detections.non-maximum suppression.:-. used to eliminate bounding
## if their confidence is lower.Moreover boxes undergo non-maximum suppression which removes redundant overlapping bounding boxes. 
##Non-maximum suppression is controlled by a parameter nmsThreshold.
#########################################################################################################################################

ap = argparse.ArgumentParser()
ap.add_argument("-i", "--image", required=True,
	help="C:/Users/Desktop/deep_learning/yolo-object-detection/yolo-object-detection/images/")
ap.add_argument("-y", "--yolo", required=True,
	help="C:/Users/Desktop/deep_learning/yolo-object-detection/yolo-object-detection/")
ap.add_argument("-c", "--confidence", type=float, default=0.5,
	help="minimum probability to filter weak detections")
ap.add_argument("-t", "--threshold", type=float, default=0.3,
	help="threshold when applyong non-maxima suppression")
args = vars(ap.parse_args())

#loading the COCO class labels YOLO model is trained on.
#The file coco.names contains all the objects for which the model has been trained. 
labelsPath = os.path.sep.join([args["yolo"], "coco.names"])
LABELS = open(labelsPath).read().strip().split("\n")

# initializing a list of colors to represent each possible class label
np.random.seed(42)
COLORS = np.random.randint(0, 255, size=(len(LABELS), 3),
	dtype="uint8")

################ NOTE  ###############################################
##deriving the paths to the YOLO weights and model configuration
##The yolov3.weights weights and yolov3.cfg, The configuration file.
##Here i have set the DNN backend to OpenCV, and the target to CPU
######################################################################

weightsPath = os.path.sep.join([args["yolo"], "yolov3.weights"])
configPath = os.path.sep.join([args["yolo"], "yolov3.cfg"])

# loading the YOLO object detector trained on THE dataset (80 classes)
##To load YOLO from disk , I have used OpenCV’s DNN function called cv2.dnn.readNetFromDarknet
print("[INFO] loading YOLO from disk...")
net = cv2.dnn.readNetFromDarknet(configPath, weightsPath)


##Loading the input image  and extracting its dimensions
image = cv2.imread(args["image"])
(H, W) = image.shape[:2]

######### NOTES ##################################################################################################################
##Determining the output layer names from the YOLO model
##The forward function in OpenCV’s Net class needs the ending layer till which it should run in the network. 
##Since i am running through the whole network, i need to identify the last layer of the network. and this API actually does
### getUnconnectedOutLayers() this gives the names of the unconnected output layers
###################################################################################################################################

ln = net.getLayerNames()
ln = [ln[i[0] - 1] for i in net.getUnconnectedOutLayers()]

#################### NOTE #################################################################################
##constructing a blob from the input image and then perform a forward
##pass of the YOLO object detector, giving us our bounding boxes and
##associated probabilities
##The input image to a neural network needs to be in a certain format called a blob.
##After a frame is read from the input image or video stream, it is passed through the blobFromImage
##Function to convert it to an input blob for the neural network.
##In this process, it scales the image pixel values to a target range of 0 to 1 using a scale factor 
##of 1/255. It also resizes the image to the given size of (416, 416) without cropping.
#############################################################################################################

blob = cv2.dnn.blobFromImage(image, 1 / 255.0, (416, 416),
	swapRB=True, crop=False)
net.setInput(blob)
start = time.time()
layerOutputs = net.forward(ln)
end = time.time()

## Displaying time information of YOLO
print("[INFO] YOLO took {:.6f} seconds".format(end - start))


################### NOTE ####################################################################################################################
##Post-processing the network’s output
##initialising our lists of detected bounding boxes, confidences, and
##class IDs, respectively
##The network outputs bounding boxes are each represented by a vector of number of classes i.e 5 elements
##The first 4 elements represent the [ centerX, centerY, width, height ].The fifth element represents the confidence 
##that the bounding box encloses an object.
##Below The box is assigned to the class corresponding to the highest score for the box.
##The highest score for a box is also called its confidence. If the confidence of a box is less than the given threshold, the bounding box 
##is dropped and not considered for further processing.
###############################################################################################################################################

boxes = []
confidences = []
classIDs = []

# here,looping over each of the layer outputs
for output in layerOutputs:
	# HERE looping over each of the detections
	for detection in output:
		# extracting the class ID and confidence (i.e., probability) of
		# the current object detection
		scores = detection[5:]
		classID = np.argmax(scores)
		confidence = scores[classID]

		# here filtering out weak predictions by ensuring the detected
		# probability is greater than the minimum probability
		if confidence > args["confidence"]:
			# scaling the bounding box coordinates back relative to the
			# size of the image, keeping in mind that YOLO actually
			# returns the center (x, y)-coordinates of the bounding
			# box followed by the boxes' width and height
			box = detection[0:4] * np.array([W, H, W, H])
			(centerX, centerY, width, height) = box.astype("int")

			# using the center (x, y)-coordinates to derive the top and
			# and left corner of the bounding box
			x = int(centerX - (width / 2))
			y = int(centerY - (height / 2))

			# updating our list of bounding box coordinates, confidences,
			# and class IDs
			boxes.append([x, y, int(width), int(height)])
			confidences.append(float(confidence))
			classIDs.append(classID)

## apply non-maxima suppression to suppress weak, overlapping bounding
## boxes
idxs = cv2.dnn.NMSBoxes(boxes, confidences, args["confidence"],
	args["threshold"])

## Model have to ensure at least one detection exists
if len(idxs) > 0:
	
	################## NOTE ##############################################################################
	## looping over the indexes
	## Here [ 'count' ] variable will keep a watch on number of different objects which has been detected ##
	######################################################################################################
	
	count = 0
	for i in idxs.flatten():
		# extracting  the bounding box coordinates [ centerX, centerY, width, height]
		(x, y) = (boxes[i][0], boxes[i][1])
		(w, h) = (boxes[i][2], boxes[i][3])

		##drawing a bounding box rectangle and label on the image
		##Finally, i am drawing the boxes that were filtered through the non maximum suppression, on the input 
		##frame with their assigned class label and confidence scores.
		color = [int(c) for c in COLORS[classIDs[i]]]
		cv2.rectangle(image, (x, y), (x + w, y + h), color, 2)
		text = "{}: {:.4f}".format(LABELS[classIDs[i]], confidences[i])
		##print('i value :-> ',i,'label name:->', LABELS[classIDs[i]])
		
		############### NOTE ################################################################################
		### Below code is checking whether there are more than one object has been detected,
		## If more than one then first iteration would be to create and write to the file the object name 
		## then in the second run  count would be 0, so other object name to be appended.
		## This object name ( file = out_item.txt) would be used by the Recommender System.
		######################################################################################################
		
		if count == 0:
			with open ('C:/Users/Desktop/deep_learning/yolo-object-detection/yolo-object-detection/out_item.txt','w') as f:
				sys.stdout = f
				print(LABELS[classIDs[i]])
				##print('count',count)
				count +=1
				f.close()
		else:
			with open ('C:/Users/Desktop/deep_learning/yolo-object-detection/yolo-object-detection/out_item.txt','a+') as file:
				sys.stdout = file
				print(LABELS[classIDs[i]])
				file.close()
		
		cv2.putText(image, text, (x, y - 5), cv2.FONT_HERSHEY_SIMPLEX,
			0.5, color, 2)

## showing the output image
cv2.imshow("Image", image)
cv2.waitKey(0)

#################  NOTE  ############################################
##print('Executing below the script for the RECOMMENDER SYSTEM !!')
## So , below invocation would start MY RECOMMENDER SYSTEM 
#####################################################################

os.system("python C:/Users/Desktop/deep_learning/Apriori.py")
#print('Script executed !!')

##########################################################################
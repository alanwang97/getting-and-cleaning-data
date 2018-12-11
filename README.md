# getting-and-cleaning-data

the script is used to process specific dataset.

The script does not follow the instruction of the coursework strictly.

First, the script reads all the raw datasets needed(you can find them in the code book) and the default working dictionary is "XXXX/UCI HAR Dataset".   (I have added some new codes to help you run the code, so you can run the script directly after replacing my path to your test path)

Second, the script replaces the number in the "train_y.txt" and "test_y"  with the activities according to the "activity_labels.txt", which is the 3rd step of the instruction.

Third, the script labels the data set with descriptive variable names, which is the 4th step of the instruction. But the labeled datasets are not merged at this stage.

Then, the script extracts only the measurements on the mean and standard deviation for each measurement, the second step in the instruction. In this way, the memory needed to merge the datasets can be saved a lot and the time to run the script can be reduced.

Next, the script merges the data of test subjects and of train subjects.

Finally, the script uses two loops to pick up the dataframe that only contains one subject and one activity and then uses mean_col function (you can find the function in the code book) to produce a vector which will be bound by the rbind function to generate the final tidy dataset.

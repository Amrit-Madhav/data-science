##########################  SOLUTION ##############################################
"""
1)	Create a python functionality to do logging to hdfs
given info
hdfs_name_node_ip='x.x.x.x'
hdfs_name_ndoe_port='1234'
hdfs_user_name='test'
"""
from pyarrow import HdfsClient


def main():
	def hdfs_write():
		hdfs_name_node_ip = 'x.x.x.x'
		hdfs_name_ndoe_port = '1234'
		hdfs_user_name = 'test'
		hdfs = HdfsClient(hdfs_name_node_ip, hdfs_name_ndoe_port, hdfs_user_name, driver='libhdfs')  # To connect Natively to HDFS using Python

		path_to_my_hdfs_file = '/apps/staging/my_dir/file'
		with hdfs.open(path_to_my_hdfs_file, 'wb') as fw:
			fw.write(data_write)


		
	data_write = 'Hive partitioned table has been overwritten'
	try:
		hdfs_write(data_write)

	except Exception as e:
		print('Exception occured', e)

if __name__ == '__main__':
	main()
	
######################################################################################################################################
 """
Q2)	Create a hive context in pyspark and set the below properties
hive.merge.mapredfiles=false
hive.merge.smallfiles.avgsize=16000000
hive.execution.engine=mr
"""
from pyspark import SparkConf,SparkContext
from pyspark.sql import SQLContext
from pyspark.sql import HiveContext
from pyspark.storageLevel import StorageLevel
from pyspark.sql.functions import col,expr,when
import sys
import os

def main(sc,load_id):
	sqlContext = HiveContext(sc)
	
	emp_table = sqlContext.sql("select emp_id,emp_name,emp_dept,gender,division from emp_table")
	emp_table.createOrReplaceTempView("emp_df")
	movies_watched = sqlContext.sql("select emp_id as emp_idm,movie_name from movies_table")
	movies_watched.createOrReplaceTempView("movies_df")
	
	movies_df.persist(StorageLevel.MEMORY_AND_DISK)  ## Persisting movies dataframe
	
	joined_df = emp_df.alias('v1'),join(movies_df.alias('v2'),col('v1.emp_id') == col('v2.emp_idm'),inner).select(col('v1.emp_id'),col('v1.dept_name') \
	,col('v2.movie_name'))
	
	joined_df_final = joined_df.repartition(len(joined_df.select(col('emp_dept')).distinct().collect()))  ## repartioning on the distinct dept_name
	joined_df_final.createOrReplaceTempView('temp_table')
	
	sqlContext.setConf("hive.merge.mapredfiles=false")
	sqlContext.setConf("hive.merge.smallfiles.avgsize=16000000")
	sqlContext.setConf("hive.execution.engine=mr")
	final_sql = 'INSERT OVERWRITE TABLE target_table select * from temp_table'
	sqlContext.sql(final_sql)
	
if __name__ == "__main__":

	load_id = sys.argv[1]
	conf = SparkConf()
	sc = SQLContext(conf=conf)
	main(sc,load_id)
	
	
	
##############################################################################################################################	
"""
3)	Assume there is a column called email in a dataframe
create a code snippet to generate a new column email_valid_flag which flags if the email is valid or not
in pandas
in pyspark
"""
#########  IN PYSPARK ##############################
from pyspark import SparkConf,SparkContext
from pyspark.sql import SQLContext
from pyspark.sql import HiveContext
from pyspark.storageLevel import StorageLevel
from pyspark.sql.functions import col,expr,when
from pyspark.sql.functions import udf
import re
import sys
import os


### Using PySpark  #####
def main(sc,load_id):
	
	sqlContext = HiveContext(sc)
	dept_df = sqlContext.sql("select name,id,email from dept_table")
	dept_df.createOrReplaceTempView('dept_df')
	
	def valid_email(email):
		if len(email) > 9:
			if re.match(".+@[a-zA-Z0-9]+.[a-zA-Z0-9]$",email) !=None:
				return Y
			
		else:
			return N
	
	udf_email = udf(valid_email,String())   #### UDF Created
	dept_wt_email_flag = dept_df.alias('d1').select(col('d1.name'),col('d1.id'),col('d1.email')).withColumn('email_valid_flag',udf_email(col('email')))

if __name__ == "__main__":

	load_id = sys.argv[1]
	conf = SparkConf()
	sc = SQLContext(conf=conf)
	main(sc,load_id)

####### IN PANDAS #############
def main(sc,load_id):
	
	sqlContext = HiveContext(sc)
	dept_df = sqlContext.sql("select name,id,email from dept_table")
	dept_df.createOrReplaceTempView('dept_df')
	dept_df_pd = dept_df.toPandas()
	dept_df_pd_copy = dept_df.toPandas()
	print('Data Shape',dept_df_pd.shape[0])
	
	######## Method Call ########################
	def valid_email_algorithm(email):
		if len(email) > 9:
			if re.match(".+@[a-zA-Z0-9]+.[a-zA-Z0-9]$",email) !=None:
				return Y
			
		else:
			return N
	
	dept_df_pd['email_valid_flag'] = dept_df_pd.apply(lambda x:valid_email_algorithm(x['email'],dept_df_pd_copy),axis =1)
	dept_df_pd.describe()
	dept_spark_df = sqlContext.createDataFrame(dept_df_pd)  ## if in case need to convert it back into spark data frame.
	dept_spark_df.write.option("compression","zlib").mode("overwrite").format("orc").save('/apps/hive/warehouse/dept.db/dept_table')

if __name__ == "__main__":

	load_id = sys.argv[1]
	conf = SparkConf()
	sc = SQLContext(conf=conf)
	main(sc,load_id)
	

###########################################################################################################################
"""
4)	Convert a dataframe to list of dicts and read the same into a another dataframe with prefix of "a_" apended to the original column name (assume some data)
      a) Code using pandas 
      b) Code using pyspark

"""

###### using Pandas #####################
import pandas as pd


#####  using Pandas  ##########
students = {'name': ['Mat jill', 'James', 'legend'],
         'marks': [150, 200, 50],
         'fees': [2000, 4000, 10000],
         'dept': ['IT', 'sales', 'R & D']}
df = pd.DataFrame.from_dict(students)  ## creating pandas dataframe from dictionary
pd_dict = df.to_dict()  ## converting Pandas dataframe to dictionary

pandas_df = pd.DataFrame.from_dict(pd_dict)  ##creating Pandas dataframe (pandas_df) from the dictionary (pd_dict)

df_prefixed = pandas_df.add_prefix('a_')  ## adding prefix to the columns
print(df_prefixed.shape[0])
print('Data: - ', df_prefixed)

######## Using PySpark   ########################
spark_df =spark.createDataFrame({'name': ['Mat jill', 'James', 'legend'],
         'marks': [150, 200, 50],
         'fees': [2000, 4000, 10000],
         'dept': ['IT', 'sales', 'R & D']}).toDf()

df_spark_dict = spark_df.toPandas().to_dict()
pandas_df = pd.DataFrame.from_dict(df_spark_dict)
spark_df_prefixed = pandas_df.add_prefix('a_')

spark_dataframe =sqlContext.createDataFrame(spark_df_prefixed)  ## created spark dataframe

###########################################################################################################

############################################################################################################################
"""
5)	Using pyspark, create snippet to generate a sequential id column in a dataframe
"""

######### USING PYSPARK ###############
from pyspark.sql.functions import monotonicallyIncreasingId

	sqlContext = HiveContext(sc)
	dept_df = sqlContext.sql("select name,id,email from dept_table")
	dept_final_df = dept_df.withColumn("id", monotonically_increasing_id())
	
##### Using PANDAS  #####################

	sqlContext = HiveContext(sc)
	dept_df = sqlContext.sql("select name,id,email from dept_table")
	dept_df_pd = dept_df.toPandas()
	dept_df_pd = dept_df_pd.reset_index(drop=True)

########################################################################################
"""
7) Write a code snippet to do pivot table of a dataframe using pandas(assume some data)
"""
from pandas import DataFrame

df_student = {
'student' : ['amrit','sam','john','amrit','sam','john']
'behaviour': ['good','bad','intell','dynamic','dynamic','good']
'total_books': ['20','30','50','10','50','30']
'score': ['90','80','50','70','50','100']
}

df_student_data = DataFrame(df_student, columns= ['student', 'behaviour','total_books','score'])

df_student_data_details = df_student_data.pivot_table(index = ['student','total_books'],columns ='behaviour',values='score').reset_index()
print('Check data : - ',df_student_data_details.shape[0])

#####################################################################################################################
"""
8) Write a code snippet to read a table from sqlitedb  into pandas dataframe(assume some data)
"""
import pandas as pd
import sqlite3

conn = sqlite3.connect('C://Desktop/user/sqlite/employee.sqlite')

df_employee = pd.read_sql_query("select * from employee;", conn)
print(df_employee.shape[0])

#########################################################################################################################
"""
9) Assume that there is directory with mix of csv files delimited by comma(,),tab(\t) and pipe(|)
they all have the same number of columns.
create a python code snippet to read the files from above directory into a pandas dataframe
and create a spark dataframe of the above pandas dataframe
"""

import glob
import pandas as pd

# get data file names

path =r'D:/AMRIT/data'
all_files = glob.glob(path + "/*.csv")

all_data_df = []

for filename in all_files:
    df = pd.read_csv(filename, index_col=None, header=0)
    all_data_df.append(df)


spark_df = sqlContext.createDataFrame(all_data_df) ## converting Pandas dataframe into Spark dataframe
spark_df.createOrReplaceTempView('spark_df')
print(spark_df.printSchema())
	


#####################################################################################################################

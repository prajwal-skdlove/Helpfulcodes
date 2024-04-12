# -*- coding: utf-8 -*-

import sys
import logging
import traceback

# Logfilename
def logfile_name(filelocation = None, filename = None):
    
    import os
    from datetime import datetime
    
    if filelocation is None:
        filelocation = os.path.dirname(os.path.realpath(__file__))
    if filename is None:
        filename = os.path.basename(os.path.realpath(__file__)).split(".")[0]
    
    current_datetime = datetime.now().strftime("%Y%m%d_%H%M%S")
    logfilename = f"{filelocation}\Log_{filename}_{current_datetime}.log"
    
    return logfilename

logfilename = logfile_name()
print(logfilename)
        

# Tracebook
def traceback_details(inputexecutesysobject):
    traceback_details = {
                        "filename": inputexecutesysobject[2].tb_frame.f_code.co_filename,
                        "lineno": inputexecutesysobject[2].tb_lineno,
                        "name" : inputexecutesysobject[2].tb_frame.f_code.co_name,
                        "type": inputexecutesysobject[0].__name__,
                        "message": inputexecutesysobject[1], # or see traceback._some_str()
                        }
    
    traceback_template = '''Traceback (most recent call last):
        File "%(filename)s", line %(lineno)s, in %(name)s
        %(type)s: %(message)s\n''' #Skipping the actual line item
                            
    return traceback_template % traceback_details


# Creat a logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
logging.basicConfig(filename=logfilename, filemode = 'w',format = "%(asctime)s - %(pathname)s - %(lineno)d -  %(levelname)s - %(message)s", datefmt = "%m/%d/%Y %I:%M:%S %p",  level=logging.DEBUG)

# logging.basicConfig(filename=logfilename, filemode = 'w', format = "%(asctime)s - %(pathname)s - %(lineno)d - %(levelname)s - %(message)s", datefmt = "%m/%d/%Y %I:%M:%S %p", level=logging.DEBUG)

# logging.Formatter("(%asctime)s - %(levelname)s - %(messages)s", datefmt = "%m/%d/%Y %I:%M:%S %p")
# logging.basicConfig(format='%(asctime)s %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p')
# logging.Formatter("(%asctime)s - %(levelname)s - %(messages)s", datefmt = "%m/%d/%Y %I:%M:%S %p")


# File handler
# file_handler = logging.FileHandler(logfilename, mode = 'w')
# file_handler.setLevel(logging.DEBUG)

# Create a formatter
# formatter = logging.Formatter("(%asctime)s - %(levelname)s - %(messages)s", datefmt = "%m/%d/%Y %I:%M:%S %p")
# file_handler.setFormatter(formatter)

# logger.addHandler(file_handler)

# Redirect steout and stderr to the logger
# class StreamToLogger:
#     def __init__(self, stream, logger, log_level = logging.INFO):
#         self.stream = stream
#         self.logger = logger
#         self.log_level = log_level
        
#     def write(self, message):
#         self.stream.write(message)
#         self.logger.log(self.log_level, message.strip())
        
#     def flush(self):
#         pass
    
# sys.stdout = StreamToLogger(sys.stdout, logger, logging.DEBUG)
# sys.stderr = StreamToLogger(sys.stderr, logger, logging.CRITICAL)
                              

try:
    print(a)
except:    
    logger.critical(traceback_details(sys.exc_info()))    
    try:
        print(traceback_details())
    except:
        logger.critical(traceback_details(sys.exc_info()))
        

        
# logger.removeHandler(file_handler)
# file_handler.close()
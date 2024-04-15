# -*- coding: utf-8 -*-

import sys
import logging.config
import logging
import yaml

#%%
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
        

#%%
# Traceback
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



#%%
        
class ConfigurableLogger:    
    
    logger = None
    def __init__(self, config_file = 'D:\CodeLibrary\Python\Helpfulcodes\Codes_Python\config_logger.yaml',
                 handlers = ['console', 'file_handler'],
                 log_files = {'file_handler' : logfile_name()}):
        self.log_files = log_files
        self.handlers = handlers
        self.load_config(config_file)
        self.setup_logger() 
       
      
    def load_config(self, config_file):
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)               
        

    def setup_logger(self):
        if self.logger is None:
            for handler in list(self.config['handlers']):
                if handler not in self.handlers:
                    del self.config['handlers'][handler]
                    self.config['root']['handlers'].remove(handler)
                    
                    for loggers in self.config['loggers']:
                        self.config['loggers'][loggers]['handlers'].remove(handler)
                                  
                else:
                    # If log_files is a dict, set the filename for each handler
                    if isinstance(self.log_files, dict):
                        if handler in self.log_files:
                            self.config['handlers'][handler]['filename'] = self.log_files[handler]
                    # If log_files is a string, set the filename for all handlers
                    elif isinstance(self.log_files, str):
                        self.config['handlers'][handler]['filename'] = self.log_files                    
            logging.config.dictConfig(self.config)                
            self.logger = logging.getLogger(__name__)

        
    def log(self, message, level = logging.DEBUG):            
        self.logger.log(level , message)

                
           
        

# Example usage
if __name__ == "__main__":
  logger = ConfigurableLogger()
  logger.log("This is INFO", logging.INFO)
  logger.log("This is DEBUG", logging.DEBUG)  
  logger.log("This is WARNING", logging.WARNING)
  logger.log("This is CRITICIAL", logging.CRITICAL)
  logger.log("This is ERROR", logging.ERROR)
  # Add exception handling for errors during logging configuration
                          

try:
    print(a)
except:    
    logger.log(traceback_details(sys.exc_info()))    
    try:
        print(traceback_details())
    except:
        logger.log(traceback_details(sys.exc_info()), logging.CRITICAL)
        

# -*- coding: utf-8 -*-

import logging
import logging.config
import yaml



config_yaml = 'D:\CodeLibrary\Python\Helpfulcodes\Codes_Python\config_logger.yaml'
log_file = 'D:\CodeLibrary\Python\Helpfulcodes\Codes_Python\Log_test.log'

        
class ConfigurableLogger:    
    
    # logger = None
    def __init__(self, config_file = 'D:\CodeLibrary\Python\Helpfulcodes\Codes_Python\config_logger.yaml',
                 handlers = ['console', 'file_handler'],
                 log_files = {'file_handler' : log_file}):
        self.log_files = log_files
        self.handlers = handlers
        self.load_config(config_file)
        self.setup_logger() 
       
      
    def load_config(self, config_file):
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)               
        

    def setup_logger(self):
        # if self.logger is None:
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

        
    def writetolog(self, message, level = logging.DEBUG):            
        self.logger.log(level , message)

                
           
        

# Example usage
if __name__ == "__main__":
  logger = ConfigurableLogger()
  logger.writetolog("This is INFO", logging.INFO)
  logger.writetolog("This is DEBUG", logging.DEBUG)  
  logger.writetolog("This is WARNING", logging.WARNING)
  logger.writetolog("This is CRITICIAL", logging.CRITICAL)
  logger.writetolog("This is ERROR", logging.ERROR)
  # Add exception handling for errors during logging configuration

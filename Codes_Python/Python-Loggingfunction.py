# -*- coding: utf-8 -*-

import logging
import logging.config
import yaml


config_yaml = 'D:\CodeLibrary\Python\Helpfulcodes\Codes_Python\config_logger.yaml'
log_file = 'D:\CodeLibrary\Python\Helpfulcodes\Codes_Python\Log_test.log'
# with open(config_yaml) as f:
#   config = yaml.safe_load(f.read())
# config['handlers']['info_file_handler']['filename'] = log_file
# config['handlers']['error_file_handler']['filename'] = log_file
# config['handlers']['debug_file_handler']['filename'] = log_file
# config['handlers']['warn_file_handler']['filename'] = log_file
# config['handlers']['critical_file_handler']['filename'] = log_file
# logging.config.dictConfig(config)

class ConfigurableLogger:
    def __init__(self, config_file, log_file):
        self.load_config(config_file)
        self.setup_logfiles(log_file)
        self.log_file = log_file
        self.setup_logger()     
      
    def load_config(self, config_file):
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
            
    def setup_logfiles(self, log_file):
        self.config['handlers']['info_file_handler']['filename'] = log_file
        self.config['handlers']['error_file_handler']['filename'] = log_file
        self.config['handlers']['debug_file_handler']['filename'] = log_file
        self.config['handlers']['critical_file_handler']['filename'] = log_file
        self.config['handlers']['warn_file_handler']['filename'] = log_file        
        

    def setup_logger(self):  
        logging.config.dictConfig(self.config)
        self.logger = logging.getLogger(__name__)    
        
    def logging(self, message, level = logging.DEBUG):    
        self.logger.log(level , message)

# Example usage
if __name__ == "__main__":
  logger = ConfigurableLogger(config_yaml, log_file)
  logger.logging("This is an info message")
  logger.logging("This is an info message", logging.DEBUG)
  logger.logging("This is an info message", logging.WARNING)
  logger.logging("This is an info message", logging.WARNING)
  logger.logging("This is a warning", logging.DEBUG)
  logger.logging("This is an error with captured output", logging.ERROR)
  # Add exception handling for errors during logging configuration

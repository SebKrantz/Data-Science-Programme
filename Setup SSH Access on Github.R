# Source: https://happygitwithr.com/hello-git.html
# After creating a project with version control, using an existing or newly created Github repository,
# in Rstudio under Git click 'More' and then 'Shell', Execute the following commands:

bash # gets git bash shell

# Create user:
git config --global user.name 'Jane Doe'
git config --global user.email 'jane@example.com'
git config --global --list

# https://happygitwithr.com/ssh-keys.html

ls -al ~/.ssh/ # If you see a pair of files like id_rsa.pub and id_rsa, you have a key pair already.
# In Rstudio Go to Tools > Global Options…> Git/SVN > Create RSA Key….
# Click “Create” and RStudio will generate an SSH key pair, stored in the files ~/.ssh/id_rsa and ~/.ssh/id_rsa.pub
  
# then in R, open shell, do 
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa  

# finally (windows specific):
git config remote.origin.url git@github.com:your_username/your_project.git
  
# Now we store a copy of your public key on GitHub.
# In Rstudio Go to Tools > Global Options…> Git/SVN. If your key pair has the usual name, id_rsa.pub and id_rsa, 
# RStudio will see it and offer to “View public key”. Do that and accept the offer to copy to your clipboard. 
# If your key pair is named differently, use another method.
  
# Make sure you’re signed into GitHub. Click on your profile pic in upper right corner and go Settings, 
# then SSH and GPG keys. Click “New SSH key”. Paste your public key in the “Key” box. Give it an informative title, 
# presumably related to the comment you used above, during key creation. For example, you might use 2018-mbp to 
# record the year and computer. Click “Add SSH key”.

# In theory, we’re done! You can use 
ssh -T git@github.com 
# to test your connection to GitHub. 
# If you’re not sure what to make of the output, see the link for details. Of course, 
# the best test is to work through the realistic usage examples elsewhere in this guide.


# Note also: For each new github repository (Project in Rstudio),
# you again need to enable SSH by executing these commands again in the git bash shell. 

bash # Get git bash command line

# Adding SSH
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa  # Type your password after executing this.
# And (only on Windows):
git config remote.origin.url git@github.com:your_username/your_project.git

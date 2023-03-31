#!/bin/bash

userName="danielthoren"
userEmail="danne_thoren456@hotmail.com"

if ! $(git config user.name | grep -q $userName);
then
    echo "Setting git config.name to: " $userName
    git config user.name $userName
fi

if ! $(git config user.email | grep -q $userEmail);
then
    echo "Setting git config.email to: " $userEmail
    git config user.email $userEmail
fi

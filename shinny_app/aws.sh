#!/bin/bash

# Configura tu número de serie MFA y tu código de token
MFA_SERIAL_NUMBER="arn:aws:iam::658275627936:user/AdminDaniel"
MFA_TOKEN_CODE="450875"

# Obtiene un token de sesión de AWS STS
AWS_SESSION=$(aws sts get-session-token --serial-number $MFA_SERIAL_NUMBER --token-code $MFA_TOKEN_CODE)

# Extrae las credenciales del token de sesión
AWS_ACCESS_KEY_ID=$(echo $AWS_SESSION | jq -r '.Credentials.AccessKeyId')
AWS_SECRET_ACCESS_KEY=$(echo $AWS_SESSION | jq -r '.Credentials.SecretAccessKey')
AWS_SESSION_TOKEN=$(echo $AWS_SESSION | jq -r '.Credentials.SessionToken')

# Imprime las credenciales
echo "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID"
echo "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY"
echo "AWS_SESSION_TOKEN=$AWS_SESSION_TOKEN"

# Pandemic Relief Distribution (PRD) Smart Contract

A Clarity smart contract for transparent distribution of relief funds during pandemic situations.

## Features

- Registration system for relief recipients
- Secure fund management
- Transparent distribution tracking
- Configurable relief amount per person
- Complete distribution history

## Contract Functions

### Administrative Functions
- `set-contract-owner`: Transfer contract ownership
- `fund-contract`: Add funds to the contract
- `set-distribution-amount`: Set relief amount per recipient
- `activate-distribution`: Start distribution period
- `deactivate-distribution`: End distribution period

### Recipient Functions
- `register-recipient`: Register for relief
- `claim-relief`: Claim allocated relief funds

### Read-Only Functions
- `get-recipient-info`: View recipient registration details
- `get-distribution-info`: View distribution records
- `get-contract-info`: View contract status

## Usage

1. Deploy contract using Clarinet
2. Fund contract with initial amount
3. Set relief amount per person
4. Allow recipients to register
5. Activate distribution
6. Recipients can claim their relief funds

## Security

- Only contract owner can modify critical parameters
- Double-claim prevention
- Sufficient funds verification
- Transparent transaction records
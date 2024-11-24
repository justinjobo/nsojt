// eslint-disable-next-line import/extensions
import { login, challengeResponse, base64DecodeUtf8 } from './loginHelper.js';

let challengeId = '';

const showChallange = () => {
    document.getElementById('login-group').classList.add('hidden');
    document.getElementById('challange-group').classList.remove('hidden');
};

const hideChallange = () => {
    document.getElementById('login-group').classList.remove('hidden');
    document.getElementById('challange-group').classList.add('hidden');
};

const doLogin = async () => {
    try {
        document.getElementById('login-error-message').textContent = '';
        const user = document.getElementById('username').value;
        const passwd = document.getElementById('password').value;
        await login({ user, passwd });
        window.location.href = '/index.html';
    } catch (error) {
        if (error.data.reason === 'Authentication challenge') {
            challengeId = error.data.challenge_id;
            showChallange();
            document.getElementById(
                'challange-message',
            ).textContent = base64DecodeUtf8(error.data.challenge_prompt);
            document.getElementById('challange').value = '';
        } else {
            document.getElementById(
                'login-error-message',
            ).textContent = `error: ${error.data.reason}`;
        }
    }
};

const doChallange = async () => {
    try {
        const response = document.getElementById('challange').value;
        await challengeResponse({ challengeId, response, ackWarning: false });
        window.location.href = '/index.html';
    } catch (error) {
        if (error.data.reason === 'Authentication challenge') {
            challengeId = error.data.challenge_id;
            document.getElementById('challange').value = '';
            document.getElementById(
                'challange-message',
            ).textContent = base64DecodeUtf8(error.data.challenge_prompt);
        } else {
            challengeId = '';
            hideChallange();
            document.getElementById(
                'login-error-message',
            ).textContent = `error: ${error.data.reason}`;
        }
    }
};


window.addEventListener('load', () => {
    document.getElementById('login-button').addEventListener('click', doLogin);
    document.getElementById(
        'challange-button',
    ).addEventListener('click', doChallange);
});

$(document).ready(function()
{
    setStatusNotConnected();

    // controls
    $("#sendButton").click(sendMessage);
    $("#connectButton").click(connect);
    $("#disconnectButton").click(disconnect);
    $("#loginButton").click(login);
    $("#getCharacterListButton").click(getCharacterList);

    // player
    $("#roomDirNW").click("nw", go);
    $("#roomDirN").click("n", go);
    $("#roomDirNE").click("ne", go);
    $("#roomDirW").click("w", go);
    $("#roomDirE").click("e", go);
    $("#roomDirSW").click("sw", go);
    $("#roomDirS").click("s", go);
    $("#roomDirSE").click("se", go);
});

function writeStatus(message)
{
    var html = document.createElement("div");
    html.setAttribute("class", "message");
    html.innerHTML = message;
    document.getElementById("console").appendChild(html);
}

// Websockets
function connect()
{
    ws = new WebSocket("ws://localhost:8081/ws.yaws");

    ws.onopen = function (evt)
    {
        writeStatus("connected");
        setStatusConnected();
    }

    ws.onclose = function (evt)
    {
        writeStatus("disconnected");
        setStatusNotConnected();
    }

    ws.onmessage = function (evt)
    {
        // Write to console
        writeStatus("response: " + evt.data);

        // Process message
        processResponse(evt.data);
    }

    ws.onerror = function (evt)
    {
        writeStatus("error: " + evt.data);
        setStatusNotConnected();
    }
}

function disconnect()
{
    ws.close();

    setStatusNotConnected();
}

function sendMessage()
{
    var msg = $("#messageField").val();
    ws.send(msg);
}

function login()
{
    var user = $("#login").val();
    var password = $("#password").val();
    ws.send('{"type":"login_request","body":{"user":"' + user + '","password":"' + password + '"}}');
    setStatusAuthenticated();
}

function getCharacterList()
{
    ws.send('{"type":"get_character_list_request","body":"none"}');
}

// FSM
var STATUS_NOT_CONNECTED = 0;
var STATUS_CONNECTED_NOT_AUTH = 1;
var STATUS_CONNECTED_AUTH = 2;
var current_status = STATUS_NOT_CONNECTED;

function setStatusNotConnected()
{
    current_status = STATUS_NOT_CONNECTED;
    setUI();
}
function setStatusConnected()
{
    current_status = STATUS_CONNECTED_NOT_AUTH;
    setUI();
}
function setStatusAuthenticated()
{
    current_status = STATUS_CONNECTED_AUTH;
    setUI();
}

function setUI()
{
    switch(current_status)
    {
        case STATUS_CONNECTED_NOT_AUTH:
            $("#connectButton").attr('disabled', 'disabled');
            $("#disconnectButton").removeAttr('disabled');
            $("#loginButton").removeAttr('disabled');
            $("#getCharacterListButton").attr('disabled', 'disabled');
            $("#sendButton").removeAttr('disabled');
            break;
        case STATUS_CONNECTED_AUTH:
            $("#connectButton").attr('disabled', 'disabled');
            $("#disconnectButton").removeAttr('disabled');
            $("#loginButton").attr('disabled', 'disabled');
            $("#getCharacterListButton").removeAttr('disabled');
            $("#sendButton").removeAttr('disabled');
            break;
        case STATUS_NOT_CONNECTED:
            $("#connectButton").removeAttr('disabled');
            $("#disconnectButton").attr('disabled', 'disabled');
            $("#loginButton").attr('disabled', 'disabled');
            $("#getCharacterListButton").attr('disabled', 'disabled');
            $("#sendButton").attr('disabled', 'disabled');
            $("#roomDirNW").attr('disabled', 'disabled');
            $("#roomDirN").attr('disabled', 'disabled');
            $("#roomDirNE").attr('disabled', 'disabled');
            $("#roomDirW").attr('disabled', 'disabled');
            $("#roomDirE").attr('disabled', 'disabled');
            $("#roomDirSW").attr('disabled', 'disabled');
            $("#roomDirS").attr('disabled', 'disabled');
            $("#roomDirSE").attr('disabled', 'disabled');
            break;
    }
}

function processResponse(data)
{
    var obj = $.parseJSON(data);
    console.log(obj);
    if(obj == null) return;

    switch (obj.type)
    {
        case "login_response":
            break;
        case "get_character_list_response":
            break;
        case "load_character_response":
            break;
        case "":
            break;
        case "":
            break;
        case "":
            break;
        case "":
            break;
    }

}

function go(direction)
{
    console.log('go: ' + direction.data);
    console.log(direction);
}
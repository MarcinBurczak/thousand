@(username: String)(implicit r: RequestHeader)

$(function() {

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var chatSocket = new WS("@routes.Application.chat(username).webSocketURL()")

    var sendMessage = function() {
        chatSocket.send(JSON.stringify(
            {type: "invite", user: $("#members").val()}
        ))
    }

    var receiveEvent = function(event) {
        var data = JSON.parse(event.data)

        // Handle errors
        if(data.error) {
            chatSocket.close()
            $("#onError span").text(data.error)
            $("#onError").show()
            return
        } else {
            $("#onChat").show()
        }

        //TODO dodanie ifologii na podstawie typu wiadomości
        // Dodanie zakładki
        var gameTabsLi = $('<li><a href="#profile" data-toggle="tab">...</a></li>')
        var gameTabsDiv = $('<div class="tab-pane" id="profile">...</div>')

        $("a", gameTabsLi).text(data.from)
        $("div", gameTabsDiv).text(data.from)

        $('#gameTabsUl').append(gameTabsLi)
        $('#gameTabsDiv').append(gameTabsDiv)

        // Update the members list
        $("#members").html('')
        $(data.members).each(function() {
            var li = document.createElement('option');
            li.textContent = this;
            $("#members").append(li);
        })
    }

    $("#inviteButton").click(sendMessage)

    chatSocket.onmessage = receiveEvent

})
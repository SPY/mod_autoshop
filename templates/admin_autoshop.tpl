{% extends "admin_base.tpl" %}

{% block title %}{_ Autoshop _}{% endblock %}

{% block content %}

{% wire id="admin-csv-import" type="submit" postback="admin_csv_import" %}
<form name="admin-csv-import" id="admin-csv-import" method="POST" action="postback">

    <div class="admin-header">

        <h2>{_ Import CSV data _}</h2>

        <p>{_ Here you can import products from CSV file. _}</p>

        <div class="well">
            <select name="provider">
                {% for id in m.search[{ query cat="provider" }] %}
                    <option value="{{ id }}">{{ m.rsc[id].title }}</option>
                {% endfor %}
            </select>
            <input type="file" id="upload_file" name="upload_file" />
            {% validate id="upload_file" type={presence} %}
            <button class="btn btn-primary b-csv-import__button" style="position: relative; top: -5px;" type="submit">{_ Import _}</button>
        </div>
    </div>
    
</form>


{% endblock %}

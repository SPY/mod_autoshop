{% extends "admin_base.tpl" %}

{% block title %}{_ Autoshop _}{% endblock %}

{% block content %}

{% wire id="admin-autoshop-csv-import" type="submit" postback="autoshop_csv_import" %}
<form name="admin-autoshop-csv-import" id="admin-autoshop-csv-import" method="POST" action="postback">

    <div class="admin-header">
        <table class="b-products__list">
            <tr>
                <th class="b-products__header">{_ Номер _}</th>
                <th class="b-products__header">{_ Наименование _}</th>
                <th class="b-products__header">{_ Цена _}</th>
                <th class="b-products__header">{_ Срок поставки _}</th>
                <th class="b-products__header"></th>
            </tr>
            {% with q.number | default:"" as num %}
            {% for id in m.autoshop[{search number=num}] %}
            <tr>
                <td class="b-products__cell">{{ m.autoshop[id].number }}</td>
                <td class="b-products__cell">{{ m.autoshop[id].title }}</td>
                <td class="b-products__cell">{{ m.autoshop[id].price | format_price }} руб.</td>
                <td class="b-products__cell">1 - 3 дня</td>
                <td class="b-products__cell">
                    <button id="{{ #add_to_basket.id }}" class="btn btn-primary b-products__in-basket setcookie" type="submit">
                        {_ В корзину _}
                    </button> 
                    {% wire id=#add_to_basket.id type="click" postback={add_to_cart id=id} delegate="detalkupi" %}
                </td>
            </tr>
            {% empty %}
            <tr> <td> {_ Деталей с заданным номером не найдено _} </td> </tr>
            {% endfor %}
            {% endwith %}
        </table>

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

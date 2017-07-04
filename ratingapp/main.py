import os
import flask
import json
import simplejson

DIRECTORY = 'output/'

app = flask.Flask(__name__,
                  static_url_path='/output/',
                  static_folder='/home/stefan/Repos/spiro/output')


def listdir():
    return sorted(os.listdir(DIRECTORY))


def first_item():
    l = listdir()
    return l[0]


def next_item(current_item):
    l = listdir()
    idx = l.index(current_item)
    return l[idx + 1]


def load_json(fn):
    with open(fn, 'r') as f:
        return json.loads(f.read())


def load_item(item_id):
    dir_ = os.path.join(DIRECTORY, item_id)
    diagram_fn = os.path.join(item_id, 'diagram.svg')
    meta_fn = os.path.join(dir_, 'meta.json')
    meta = load_json(meta_fn)
    return diagram_fn, meta


def save_feedback(feedback, item_id):
    _, meta = load_item(item_id)
    dir_ = os.path.join(DIRECTORY, item_id)
    meta_fn = os.path.join(dir_, 'meta.json')
    meta.update(feedback)
    with open(meta_fn, 'w') as f:
        f.write(simplejson.dumps(meta))


@app.route('/output/<path:path>')
def send_output(path):
    return app.send_static_file(path)


@app.route('/nextitem', methods=['POST', 'GET'])
def show_item():
    d = (dict(flask.request.form))
    for k, v in d.items():
        d[k] = v[0]

    like = d.get('like') is not None

    if d.get('current_item') is not None:
        save_feedback({'like': like}, item_id=d.get('current_item'))
        current_item = next_item(d.get('current_item'))
        diagram_fn, meta = load_item(current_item)
    else:
        current_item = first_item()
        diagram_fn, meta = load_item(current_item)
        current_item=os.path.dirname(diagram_fn)

    html = flask.render_template_string('''
<body>
<img src="{{diagram_url}}"/>
<form action="/nextitem" method="post" target="_self">
<input name="like" type="submit" value="like">
<span style="display: inline-block; width: 500px"></span>
<input name="dont_like" type="submit" value="dont_like">
<input name="current_item" type="hidden" value="{{current_item}}">
</form>
<pre>
{{meta}}
</pre>
</body>
    ''',
    diagram_url=flask.url_for('send_output', path=diagram_fn),
    current_item=current_item,
    meta=simplejson.dumps(meta))
    return html


def debug():
    app.run('0.0.0.0', port=8080)


def find_jsons(directory):
    dirs_ = os.listdir(directory)
    fns = []
    for dir_ in dirs_:
        fn = os.path.join(directory, dir_, 'meta.json')
        fns.append(fn)
    return fns


def collect_likes(directory):
    ts = []
    for fn in find_jsons(directory):
        d = load_json(fn)
        if d.get('like', False):
            ts.append(d['turnSpeeds'])
    return ts

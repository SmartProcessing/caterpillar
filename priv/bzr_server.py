#!/usr/bin/env python

import os
import gc
import sys
import simplejson
import traceback
from bzrlib import branch, repository, log, diff, revisionspec, export, errors
from bzrlib import revisionspec
from StringIO import StringIO


class EOF(Exception):
    pass


class Handler(object):
    def __init__(self):
        self.init_log()
        self.In = os.fdopen(3, 'r')
        self.Out = os.fdopen(4, 'w')


    def log_msg(self, msg, with_stack=False):
        import datetime
        if with_stack:
            exc_type, exc_value, exc_trace = sys.exc_info()
            traceback.print_tb(exc_trace, None, self.log)
        self.log.write("%s: " % datetime.datetime.now())
        self.log.write(msg + '\n')
        self.log.flush()


    def init_log(self):
        if len(sys.argv) > 1:
            cmd = sys.argv[1]
            if cmd.startswith('with_log'):
                cmd = cmd.split('=')
                if len(cmd) == 2:
                    path = cmd[1]
                else:
                    path = "/var/lib/buildnet/buildnet_master/log.log"
                self.log = open(path, "a+")
            else:
                self.log = open("/dev/null", "w")
        else:
            self.log = open("/dev/null", "w")


    def do(self):
        buf = []
        while True:
            d = self.In.read(1)
            if d == '\x00':
                msg, buf = ''.join(buf), []
                response = self.handle(msg)
                self.write_response(response)
            elif not d:
                raise EOF("eof")
            else:
                buf.append(d)


    def handle(self, msg):
        msg = simplejson.loads(msg)
        cmd = msg['cmd']
        del msg['cmd']
        try:
            result = getattr(self, "cmd_%s" % cmd)(**msg)
        except Exception, err:
            result = {'error': "%s" % err}
            self.log_msg("%s" % result, with_stack=True)
        return result


    def write_response(self, result):
        try:
            result = simplejson.dumps(result)
        except:
            result = simplejson.dumps({'error': "couldn't encode result"})
        self.Out.write(result)
        self.Out.write("\x00")
        self.Out.flush()
        gc.collect()


    def open_branch(self, path):
        return branch.Branch.open(path)


    def decode(self, result):
        for enc in ('utf-8', 'cp1251', 'latin1'):
            try:
                return result.decode(enc)
            except Exception, err:
                self.log_msg("%s" % err, with_stack=True) 
        return result


    def cmd_get_revno(self, repo_path, repo, branch):
        B = self.open_branch(get_abspath(repo_path, repo, branch))
        return {'ok': B.revno()}


    def cmd_get_changelog(self, repo_path, repo, branch, old_revno, new_revno):
        B = self.open_branch(get_abspath(repo_path, repo, branch))
        current_revno = B.revno()
        if current_revno < old_revno:
            new_revno = old_revno = current_revno
        else:
            old_revno += 1

        out = StringIO()
        lf = log.log_formatter("long", to_file=out)
        if old_revno == 0:
            old_revno = 1
        if new_revno == 0:
            new_revno = 1
        try:
            log.show_log(
                B, lf, start_revision=old_revno,
                end_revision=new_revno, verbose=True
            )
        except Exception, err:
            return {'error': '%s' % err}

        return {'ok': self.decode(out.getvalue().rstrip())}


    def cmd_get_diff(self, repo_path, repo, branch, old_revno, new_revno):
        abspath = get_abspath(repo_path, repo, branch)
        B = self.open_branch(abspath)
        current_revno = B.revno()
        if current_revno < old_revno:
            new_revno = current_revno
            old_revno = new_revno - 1
        try:
            os.chdir(abspath)
        except:
            return {'error': 'bad branch'}
        out = StringIO()
        old_label = new_label = ''
        revision = [revisionspec.RevisionSpec.from_string("%s" % x) for x in
            [old_revno, new_revno]
        ]
        (old_t, new_t, old_b, new_b, spec_files, extra_t) = \
                diff.get_trees_and_branches_to_diff(
                    None, revision, old_label, new_label
                )
        diff.show_diff_trees(old_t, new_t, out, specific_files=spec_files,
                        external_diff_options=None, old_label=old_label,
                        new_label=new_label, extra_trees=extra_t
        )
        result = out.getvalue().rstrip()
        return {'ok': self.decode(result)}


    def cmd_checkout_branch(self, repo_path, repo, branch):
        return {'error': 'not_implemented'}


    def cmd_is_repository(self, repo_path, repo):
        abspath = get_abspath(repo_path, repo)
        try:
            repository.Repository.open(abspath)
            return {'ok': ''}
        except Exception, err:
            return {'error': "not repository"}


    def cmd_is_branch(self, repo_path, repo, branch):
        abspath = get_abspath(repo_path, repo, branch)
        try:
            self.open_branch(abspath)
            return {'ok': 'ok'}
        except:
            return {'error': 'not branch'}


    def cmd_get_branches(self, repo_path, repo):
        abspath = get_abspath(repo_path, repo)
        dirs = os.listdir(abspath)
        branches = []
        for dir in dirs:
            try:
                self.open_branch(get_abspath(abspath, dir))
                branches.append(dir)
            except:
                continue
        return {'ok': branches}


    def cmd_export_branch(self, repo_path, repo, branch, export_path):
        abspath = get_abspath(repo_path, repo, branch)
        branch = self.open_branch(abspath)
        rev_tree = branch.basis_tree()
        export.export(rev_tree, export_path)
        return {'ok': 'exported'}


def get_abspath(*parts):
    return os.path.join(*parts)


def main():
    handler = Handler()
    while True:
        try:
            handler.do()
        except EOF, err:
            break
        except Exception, err:
            pass
    handler.log_msg("%s" % err)



if __name__ == '__main__':
    main()

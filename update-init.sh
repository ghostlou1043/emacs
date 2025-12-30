#!/usr/bin/env bash

# === 1. 配置上游地址 ===
# 请将下面的链接替换为上游仓库中对应文件的 "Raw" 链接
# (在 GitHub 上打开文件 -> 点击 "Raw" 按钮 -> 复制浏览器地址栏的链接)
UPSTREAM_INIT_URL="https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/refs/heads/main/init.el"
UPSTREAM_EARLY_INIT_URL="https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/refs/heads/main/early-init.el"

# === 2. 开始同步 ===
echo ">>> 正在从上游获取最新配置..."

# 使用 curl 下载并直接覆盖本地文件
# -L: 允许重定向 (GitHub Raw 链接有时需要)
# -o: 指定输出文件名 (必须严格匹配 Emacs 的标准文件名)

echo "  -> Downloading init.el..."
curl -L "$UPSTREAM_INIT_URL" -o init.el

echo "  -> Downloading early-init.el..."
curl -L "$UPSTREAM_EARLY_INIT_URL" -o early-init.el

# === 3. 完成提示 ===
echo ">>> 同步完成！"
echo ">>> 本地的 init.el 和 early-init.el 已被覆盖。"
echo ">>> 请运行 'git diff' 查看上游发生了什么变化，确认无误后提交。"

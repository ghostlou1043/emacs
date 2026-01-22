#!/usr/bin/env bash

# 脚本名称: update.sh
# 目的: 管理 Emacs 配置更新和 ewt-rs 模块安装
# 
# 使用方法:
#   ./update.sh init   - 更新 init.el 和 early-init.el
#   ./update.sh ewt    - 下载并安装 ewt-rs 模块
#   ./update.sh -h     - 显示帮助信息

# --- 全局设置 ---
# 确保任何命令失败时脚本立即退出 (全局生效)
set -e

# ==========================================
# 函数 1: 更新 init.el 和 early-init.el
# ==========================================
function do_init() {
    echo ">>> [模式: init] 准备更新配置文件..."

    # === 配置上游地址 ===
    UPSTREAM_INIT_URL="https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/refs/heads/main/init.el"
    UPSTREAM_EARLY_INIT_URL="https://raw.githubusercontent.com/jamescherti/minimal-emacs.d/refs/heads/main/early-init.el"

    # === 开始同步 ===
    echo ">>> 正在从上游获取最新配置..."

    echo "  -> Downloading init.el..."
    curl -L "$UPSTREAM_INIT_URL" -o init.el

    echo "  -> Downloading early-init.el..."
    curl -L "$UPSTREAM_EARLY_INIT_URL" -o early-init.el

    # === 完成提示 ===
    echo ">>> 同步完成！"
    echo ">>> 本地的 init.el 和 early-init.el 已被覆盖。"
    echo ">>> 请运行 'git diff' 查看上游发生了什么变化，确认无误后提交。"
}

# ==========================================
# 函数 2: 更新 ewt-rs 模块
# ==========================================
function do_ewt() {
    echo ">>> [模式: ewt] 准备安装 ewt-rs 模块..."

    # --- 配置变量 ---
    DOWNLOAD_URL="https://github.com/Master-Hash/ewt-rs/releases/download/v0.4.0/libewt-icu_segmenter-x86_64-unknown-linux-gnu.zip"
    ZIP_FILE="libewt-icu_segmenter.zip"
    EXTRACT_DIR="ewt-rs_extracted"
    TARGET_MODULES_DIR="modules"
    FINAL_SO_NAME="libewt.so"

    echo "🚀 开始安装 ewt-rs Emacs 动态模块..."

    # 1. 检查必要的工具
    echo "🔍 检查 'curl' 和 'unzip' 是否已安装..."
    if ! command -v curl &> /dev/null; then
        echo "❌ 错误: 'curl' 未安装。请先安装 'curl'。"
        exit 1
    fi
    if ! command -v unzip &> /dev/null; then
        echo "❌ 错误: 'unzip' 未安装。请先安装 'unzip'。"
        exit 1
    fi
    echo "✅ 'curl' 和 'unzip' 已安装。"

    # 2. 创建目标模块目录
    echo "📂 检查或创建目标模块目录: ${TARGET_MODULES_DIR}"
    mkdir -p "${TARGET_MODULES_DIR}"
    echo "✅ 目标模块目录已准备就绪。"

    # 3. 下载 zip 文件
    echo "⬇️ 正在从 ${DOWNLOAD_URL} 下载 ${ZIP_FILE}..."
    curl -L -o "${ZIP_FILE}" "${DOWNLOAD_URL}"
    echo "✅ 下载完成: ${ZIP_FILE}"

    # 4. 解压 zip 文件
    echo "📦 正在解压 ${ZIP_FILE} 到 ${EXTRACT_DIR}..."
    unzip -q "${ZIP_FILE}" -d "${EXTRACT_DIR}"
    echo "✅ 解压完成。"

    # 5. 查找 .so 文件并移动
    echo "🔎 正在查找解压后的 .so 文件并移动到 ${TARGET_MODULES_DIR}/${FINAL_SO_NAME}..."
    FOUND_SO_PATH=$(find "${EXTRACT_DIR}" -name "*.so" -print -quit)
    if [ -z "${FOUND_SO_PATH}" ]; then
        echo "❌ 错误: 未在解压目录中找到 .so 文件。"
        exit 1
    fi
    echo "找到 .so 文件: ${FOUND_SO_PATH}"
    mv "${FOUND_SO_PATH}" "${TARGET_MODULES_DIR}/${FINAL_SO_NAME}"
    echo "✅ .so 文件已成功移动并重命名为 ${FINAL_SO_NAME}。"

    # 6. 清理
    echo "🧹 正在清理临时文件和目录..."
    rm "${ZIP_FILE}"
    rm -rf "${EXTRACT_DIR}"
    echo "✅ 清理完成。"

    echo "🎉 ewt-rs 模块安装成功！"
    echo "模块文件已放置在: ${TARGET_MODULES_DIR}/${FINAL_SO_NAME}"
}

# ==========================================
# 函数 3: 显示帮助信息
# ==========================================
function show_help() {
    echo "用法: ./update.sh [命令]"
    echo ""
    echo "命令列表:"
    echo "  init    仅更新 init.el 和 early-init.el (从上游覆盖)"
    echo "  ewt     仅下载并安装 ewt-rs 动态模块 (libewt.so)"
    echo "  -h      显示此帮助信息"
    echo ""
    echo "示例:"
    echo "  ./update.sh init"
    echo "  ./update.sh ewt"
}

# ==========================================
# 主逻辑入口 (Main Logic)
# ==========================================

# $1 代表脚本接收到的第一个参数
case "$1" in
    init)
        do_init
        ;;
    ewt)
        do_ewt
        ;;
    -h|--help)
        show_help
        ;;
    *)
        # 如果输入的参数不是上面任何一个，或者没有输入参数
        echo "❌ 错误: 未知命令或缺少参数。"
        echo ""
        show_help
        exit 1
        ;;
esac
